#' @title Plot an ea_spatial object with multiple styles
#' @rdname plot-ea_spatial
#' @description
#' Creates a spatial plot for an `ea_spatial` object. Styles:
#'   * "fill": gridded data filled by value (also accepts "default")
#'   * "contour": contour lines of value; either by timestep or timeseries average.
#'   * "area": solid shape for all non-NA values (for pairing with ea.subset.spatial.values())
#'   * "bubble": bubble plot (point size ∝ value)
#'   * "trend": the linear trend of the variable per raster cell
#'
#' @param x An `ea_spatial` object.
#' @param y Ignored. Included for S4 generic consistency.
#' @param style Character; one of "fill", "contour", "area", or "bubble".
#' @param coastline logical. Should coastline layer be plotted? (default: TRUE)
#' @param coastline_region Character or numeric vector. Region for coastline:
#'   - "shape": to the bounding box of the spatial data object
#'   - "nw_atlantic": Northwest Atlantic
#'   - "pacific": Pacific region
#'   - "gsl": Gulf of St. Lawrence
#'   - "arctic": Arctic region
#'   - c(xmin, xmax, ymin, ymax): Custom bounding box
#'   - blank: defaults to a 5° buffer around spatial data object
#' @param coastline_color Color for coastline (default: "darkgrey")
#' @param coastline_fill Fill color for land (default: "grey90")
#' @param coastline_size Line width for coastline (default: 0.3)
#' @param coastline_resolution Resolution for coastline data: "low", "medium", "high" (default: "medium")
#' @param value_col if applicable, the column within the spatial data to plot
#' @param contour.type Character; type of contour to plot:
#'    - "timestep": creates a contour per time step in spatial dat
#'    - "aggregate": creates a contour of timeseries-averaged values
#' @param contour.bw Bin width of contour plots
#' @param ... Additional args passed to the geoms.
#' @return A ggplot object.
#' 
setGeneric("plot")

#' @rdname plot-ea_spatial
#' @export

setMethod("plot", signature(x = "ea_spatial", y = "missing"),
          function(x,
               style = c("fill", "default", "contour", "area", "bubble", "trend"),
               coastline = TRUE,
               coastline_region = NULL,
               coastline_color = "darkgrey",
               coastline_fill = "grey90",
               coastline_size = 0.3,
               point_size = .3,
               coastline_resolution = "medium",
               value_col = NULL,
               contour.type = "timestep",
               contour.bw = 2,
               ...) {
  
  
  # get data object
  df <- x@data
  # get meta object
  meta <- x@meta
  
  # class check
  if(!inherits(df, c("SpatRaster","stars","sf")))
    stop("Unsupported spatial class.")
  
  
  # Class-specific dispatcher to prepare data
  if(inherits(df, "SpatRaster")) {
    #df <- prepare_spatraster_data(df, value_col)
  } else  if(inherits(df, "stars")) {
    df <- prepare_stars_data(df, value_col)
  } else if(inherits(df, "sf")) {
    df <- prepare_sf_data(df, value_col)
  } 
  
  # Initiate plot
  p <- ggplot2::ggplot() +
    ggplot2::theme_bw()
  
  # Class-specific dispatcher
  # see individual R scripts for these functions
  if (inherits(df, "SpatRaster")) {
    p <- plot_spatraster(p, df, style, contour.type, contour.bw)
  } else if (inherits(df, "stars")) {
    p <- plot_stars(p, df, style, contour.type, contour.bw, value_col)
  } else if (inherits(df, "sf")) {
    p <- plot_sf(p, df, style, point_size = point_size, contour.type = contour.type, contour.bw = contour.bw)
  } 
  
  # add coastline if requested
  if (coastline) {
    coastline_data <- NULL
    data_bbox <- get_data_bbox(df)
    coastline_data <- get_coastline_data(
      region = coastline_region,
      data_bbox = data_bbox,
      resolution = coastline_resolution
    )
    p <- p + ggplot2::geom_sf(
      data = coastline_data,
      linewidth = coastline_size,
      fill = coastline_fill,
      color = coastline_color,
      inherit.aes = FALSE
    )
  }
  
  # add common labels
  p <- add_labs(p, style, df, meta)
  
  
  return(p)
})


# add common labels to plot
add_labs <- function(p, style, df, meta){
  
  filllab <- if(inherits(df,c("SpatRaster","stars"))) meta$units else NULL
  colorlab <- if(inherits(df,c("SpatRaster","stars"))) NULL else meta$units
  
  p <- p +
    ggplot2::coord_sf(crs = sf::st_crs(df), datum = sf::st_crs(4326)) +
    ggplot2::labs(
      title = meta$data_type,
      subtitle = paste(meta$time_descriptor, "| Source:", meta$source_citation),
      x = NULL, 
      y = NULL,
      fill = filllab,
      color = colorlab
    )
  
  return(p)
  
}



# # Helper function for contour plots -- JL EDIT
# create_contour_plot_JL <- function(df, meta, contour.time, ...) {
#   
#   if(inherits(df,"spatRaster")){
#     print("In Development.")
#   }
#   
#   if(inherits(df,"sf")){
#     print("In Development.")
#   }
#   
#   if (inherits(df, "stars")) {
#     
#     if(contour.time == "aggregate"){
#       
#       # create contours with stars native:
#       spatial_dims <- intersect(c("x","y","latitude","longitude"),names(stars::st_dimensions(df)))
#       s_mean <- stars::st_apply(df["value"], spatial_dims, mean, na.rm = TRUE)
#       names(s_mean) <- "value"
#       
#       min <- as.numeric(floor(min(s_mean$value, na.rm=T)))
#       max <- as.numeric(ceiling(max(s_mean$value, na.rm=T)))
#       
#       if((max-min)<15){
#         breaks <-  seq(min, max)
#       } else {
#         breaks <- seq(min, max, length.out=15) %>% round()
#       }
#       
#       contours <- stars::st_contour(s_mean, breaks = breaks)
#       
#       # plot
#       ggplot2::ggplot() +
#         ggplot2::geom_sf(data = contours,
#                          ggplot2::aes(fill = .data$value)) +
#         ggplot2::scale_fill_viridis_d() +
#         ggplot2::coord_sf()
#       
#     } else if (contour.time == "timestep"){
#       
#       # get time values
#       times <- stars::st_get_dimension_values(df, "time")
#       
#       min <- as.numeric(floor(min(df$value, na.rm=T)))
#       max <- as.numeric(ceiling(max(df$value, na.rm=T)))
#       
#       if((max-min)<15){
#         breaks <-  seq(min, max)
#       } else {
#         breaks <- seq(min, max, length.out=15) %>% round()
#       }
#       
#       
#       # generate contours for every timestep
#       contours <- lapply(seq_along(times), function(i) {
#         
#         # extract 2D spatial slice
#         slice <- df["value", , , i, drop = TRUE]
#         
#         # generate contours
#         out <- stars::st_contour(
#           slice,
#           breaks = breaks
#         )
#         
#         # Retain the timestep for faceting
#         out$time <- times[i]
#         out
#       })
#       
#       
#       contours <- dplyr::bind_rows(contours)
#       
#       # get existing levels ---
#       levs <- levels(contours$value)
#       # order by lower bound
#       lower <- as.numeric(sub("^\\[([^,]+),.*", "\\1", levs))
#       contours$value <- factor(
#         contours$value,
#         levels = levs[order(lower)],
#         ordered = TRUE
#       )
#       
#       
#       # Return this ggplot object from your plotting function
#       ggplot2::ggplot(contours) +
#         ggplot2::geom_sf(ggplot2::aes(fill = .data$value)) +
#         ggplot2::scale_fill_viridis_d() +
#         ggplot2::coord_sf()
#       
#     }
#   }
# }


 







# -------------------------------------------------------------------------


# -------------------------------------------------------------------------


# -------------------------------------------------------------------------

## Main S4 method
#setMethod("plot", 
#          signature(x = "ea_spatial", y = "missing"),
#          function(x, style = "fill", ...) {
#            
#            df <- x@data
#            
#            if (inherits(df, "SpatRaster")) {
#              p <- plot_spatraster(df, style, ...)
#            } else if (inherits(df, "stars")) {
#              p <- plot_stars(df, style, ...)
#            } else if (inherits(df, "sf")) {
#              p <- plot_sf(df, style, ...)
#            } else {
#              stop("Unsupported spatial class.")
#            }
#            
#            # Shared formatting
#            p <- p + ggplot2::theme_bw()
#            
#            # Add coastlines, labels, etc. here
#            
#            p
#          }
#)
#
## Class-specific dispatcher
#plot_spatraster <- function(df, style, ...) {
#  switch(
#    style,
#    fill = plot_spatraster.fill(df, ...),
#    contour = plot_spatraster.contour(df, ...),
#    contour_mean = plot_spatraster.contour_mean(df, ...),
#    area = plot_spatraster.area(df, ...),
#    stop("Unsupported plot style.")
#  )
#}
#
## Individual plotting implementation
#plot_spatraster.fill <- function(df, ...) {
#  ggplot2::ggplot() +
#    tidyterra::geom_spatraster(data = df) +
#    ggplot2::facet_wrap(~lyr) +
#    ggplot2::scale_fill_viridis_c(na.value = "transparent")
#}
#
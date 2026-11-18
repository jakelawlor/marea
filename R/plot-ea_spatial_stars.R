# script for plot-ea_spatial internals for stars objects
# (functions feed into plot-ea_spatial parent)

# class-specific dispatcher for stars plots ------------------------------
plot_stars <- function(p, df, style, contour.type, contour.bw, value_col){
  
  p <- switch(style,
              default = plot_stars_fill(p, df),
              fill = plot_stars_fill(p, df),
              contour = plot_stars_contour(p, df, contour.bw, contour.type),
              area = plot_stars_area(p, df),
              trend = plot_stars_trend(p, df)
  )
  
}

# plot stars data using cells filled by value ----------------------------
plot_stars_fill <- function(p, df){
  
  p <- p + 
    stars::geom_stars(data = df, aes(fill = value))+
    ggplot2::scale_fill_viridis_c(na.value = "transparent")
  return(p)
  
}

# plot stars data as contour of individual layers ------------------------
plot_stars_contour <- function(p, df, contour.bw = contour.bw, contour.type = contour.type){
  
  if(contour.type == "timestep"){ 
    
    stars_contour <- as.data.frame(df, xy=T)
    
    p <- p + 
      ggplot2::geom_contour_filled(
        data = stars_contour,
        aes(x = longitude, y = latitude, z = value),
        binwidth = contour.bw, 
        na.rm=T) + 
      coord_equal() 

    message("contour.time = `timestep` may result in multiple overlapping rasters. Use facet to separate timepoints.")
    
    return(p)
    
  }
  else if(contour.type == "aggregate"){
    
    # find overall mean
    df_mean <- stars::st_apply(df["value"],
                        setdiff(names(stars::st_dimensions(df)), "time"),
                        mean,
                        na.rm=T,
                        rename = F) 
    df_mean <-  as.data.frame(df_mean, xy=T)
    
    p <- p + 
      ggplot2::geom_contour_filled(
        data = df_mean,
        aes(x = longitude, y = latitude, z = value),
        binwidth = contour.bw,
        na.rm=T) + 
      ggplot2::coord_equal()

    
    return(p)
    
  } else {
    
    print("Contour type not supported. Must be `timestep` or `aggregate`.")
    
  }
  
}


# plot stars data as filled shape of all non-NA cells --------------------
# (most relevant when spatial data have been filtered to a data threshold)
plot_stars_area <- function(p, df){
  
  df_area <- df
  df_area[[1]] <- ifelse(is.na(df[[1]]), NA_real_, 1)
  
  p <- p + 
    stars::geom_stars(data = df_area) +
    ggplot2::scale_fill_gradient(
      low = "steelblue", high = "steelblue",
      na.value = "transparent", guide = "none"
    )
  
  return(p)
  
}



# plot stars trends (linear change per timestep) --------------------
# most relevant for timeseries data
plot_stars_trend <- function(p, df) {
  
  # Calculate linear slope, preserving missing time steps
  gradFun <- function(x, th) {
    t <- seq_along(x)
    keep <- !is.na(x)
    
    if (sum(keep) > th && sum(keep) >= 2) {
      unname(coef(lm(x[keep] ~ t[keep]))[2])
    } else {
      NA_real_
    }
  }
  
  # Identify spatial dimensions
  dims <- names(stars::st_dimensions(df))
  spatial_dims <- intersect(c("longitude", "latitude"), dims)  

  
  if (length(spatial_dims) != 2) {
    stop("Expected spatial dimensions named 'longitude' and 'latitude'.")
  }
  
  # Calculate trend across time for each cell
  trend <- stars::st_apply(
    df,
    MARGIN = spatial_dims,
    FUN = gradFun,
    th = 0.25 * prod(
      stars::st_dimensions(df)[
        setdiff(dims, spatial_dims)
      ] |> lapply(function(x) x$to - x$from + 1) |>
        unlist()
    )
  )
  
  names(trend) <- "Trend"
  
  # Plot trends
  p +
    stars::geom_stars(data = trend) +
    ggplot2::scale_fill_gradient2(
      high = "tomato2",
      low = "cornflowerblue",
      mid = "grey95",
      midpoint = 0,
      na.value = "transparent"
    )
}



# helper for stars prep ---------------------------------------------------
prepare_stars_data <- function(stars_obj, value_col) {
  
  # Get the first attribute name if no 'value' attribute exists
  attr_names <- names(stars_obj)
  
  if (is.null(value_col)){
    # Rename value column
    val_ind <- grep(attr_names, pattern = '_value$')
    
    if (length(val_ind) == 0) {
      stop("No value column found in data. Value column must end with '_value'.", call. = FALSE)
    } else if (length(val_ind) > 1) {
      warning("Multiple value columns found. Using the first one: ", attr_names[val_ind[1]], call. = FALSE)
      
      names(stars_obj)[val_ind[1]] <- "value"
      
    } else {
      names(stars_obj)[val_ind] <- "value"
    }
  } else{
    
    names(stars_obj)[which(names(stars_obj) == value_col)] <- "value"
    
  }
  
  # remove units
  if(!is.null(attr(stars_obj$value,"units"))){
    stars_obj[["value"]] <- units::drop_units(stars_obj[["value"]])
  }
  
  # rename x and y to latitude and longitude for downstream consistency
  if ("x" %in% names(stars::st_dimensions(stars_obj)))
    stars_obj <- stars::st_set_dimensions(stars_obj, "x", names = "longitude")
  if ("y" %in% names(stars::st_dimensions(stars_obj)))
    stars_obj <- stars::st_set_dimensions(stars_obj, "y", names = "latitude")
  
  # remove other columns besides "value"
  stars_obj <- stars_obj["value"]
  
  return(stars_obj)
}




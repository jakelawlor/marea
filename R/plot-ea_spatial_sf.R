# script for plot-ea_spatial internals for sf objects
# (functions feed into plot-ea_spatial parent)

# class-specific dispatcher for sf plots ------------------------------
plot_sf <- function(p, df, style, point_size = point_size, contour.type, contour.bw){
  
  p <- switch(style,
              default = plot_sf_fill(p, df, point_size),
              fill = plot_sf_fill(p, df, point_size),
              contour = plot_sf_contour(p, df, point_size, contour.bw, contour.type),
              area = plot_sf_area(p, df, point_size),
              bubble = plot_sf_bubble(p, df)
  )
  
}

# plot sf data using cells filled by value ----------------------------
plot_sf_fill <- function(p, df, point_size){
  
  p <- p + 
    ggplot2::geom_sf(data = df, aes(color = value),
                     size = point_size) +
    ggplot2::scale_color_viridis_c(na.value = "transparent")
  return(p)
  
}

# plot sf data as contour of individual layers ------------------------
plot_sf_contour <- function(p, df, point_size, contour.bw = contour.bw, contour.type = contour.type){
  
  if(contour.type == "timestep"){
    
    stop("sf contour in development")
    
  }
  else if(contour.type == "aggregate"){
    
    stop("sf contour in development")

  } else {
    
    print("Contour type not supported. Must be `timestep` or `aggregate`.")
    
  }
  
}


# plot sf data as filled shape of all non-NA cells --------------------
# (most relevant when spatial data have been filtered to a data threshold)
plot_sf_area <- function(p, df, point_size){
  
  p <- p +
    ggplot2::geom_sf(data = df[!is.na(df$value), ],
                     color = "steelblue",
                     size = point_size)
  return(p)
  
}


# plot sf bubble (sized by value)
plot_sf_bubble <- function(p, df){
  
  p <- p + 
    ggplot2::geom_sf(data = df,
                     aes(size = value,
                         color = value)) +
    scale_color_viridis_c()
  
  return(p)
  
}



# helper for sf prep ---------------------------------------------------
prepare_sf_data <- function(sf_obj, value_col) {
  
  # Get the first attribute name if no 'value' attribute exists
  attr_names <- names(sf_obj)
  
  if (is.null(value_col)){
    # Rename value column
    val_ind <- grep(attr_names, pattern = '_value$')
    
    if (length(val_ind) == 0) {
      stop("No value column found in data. Value column must end with '_value'.", call. = FALSE)
    } else if (length(val_ind) > 1) {
      warning("Multiple value columns found. Using the first one: ", attr_names[val_ind[1]], call. = FALSE)
      
      names(sf_obj)[val_ind[1]] <- "value"
      
    } else {
      names(sf_obj)[val_ind] <- "value"
    }
  } else{
    
    names(sf_obj)[which(names(sf_obj) == value_col)] <- "value"
    
  }
  
  # remove units
  if(!is.null(attr(sf_obj$value,"units"))){
    sf_obj[["value"]] <- units::drop_units(sf_obj[["value"]])
  }
  
  return(sf_obj)
}






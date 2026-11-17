# script for plot-ea_spatial internals for spatraster objects
# (functions feed into plot-ea_spatial parent)

# class-specific dispatcher for SpatRaster plots ------------------------------
plot_spatraster <- function(p, df, style, contour.type, contour.bw){
  
  p <- switch(style,
              default = plot_spatraster_fill(p, df),
              fill = plot_spatraster_fill(p, df),
              contour = plot_spatraster_contour(p, df, contour.bw, contour.type),
              area = plot_spatraster_area(p, df)
              )
  
}

# plot SpatRaster data using cells filled by value ----------------------------
plot_spatraster_fill <- function(p, df){
  
  p <- p + 
    tidyterra::geom_spatraster(data = df) +
    ggplot2::scale_fill_viridis_c(na.value = "transparent")
  return(p)
  
}

# plot SpatRaster data as contour of individual layers ------------------------
plot_spatraster_contour <- function(p, df, contour.bw = contour.bw, contour.type = contour.type){
  
  if(contour.type == "timestep"){
    
    p <- p +
      tidyterra::geom_spatraster_contour_filled(data = df, binwidth = contour.bw) 
    message("contour.time = `timestep` may result in multiple overlapping rasters. Use facet to separate timepoints.")
    return(p)
    
  }
  else if(contour.type == "aggregate"){
    
    p <- p + 
      tidyterra::geom_spatraster_contour_filled(data = terra::app(df, mean, na.rm=T), binwidth = contour.bw)
    return(p)
    
  } else {
    
    print("Contour type not supported. Must be `timestep` or `aggregate`.")
    
  }
  
}


# plot SpatRaster data as filled shape of all non-NA cells --------------------
# (most relevant when spatial data have been filtered to a data threshold)
plot_spatraster_area <- function(p, df){
  
  p <- p +
    tidyterra::geom_spatraster(data = terra::ifel(!is.na(df), TRUE, NA)) +
    ggplot2::facet_wrap(~lyr) +
    ggplot2::scale_fill_gradient(
      low = "steelblue", high = "steelblue",
      na.value = "transparent", guide = "none"
    )
  return(p)
  
}



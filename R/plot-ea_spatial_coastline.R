# helper functions for plot-ea_spatial: coastline


# get data bbox
get_data_bbox <- function(df) {
  
  if (inherits(df, "SpatRaster")) {
    bbox <- sf::st_bbox(
      sf::st_as_sf(terra::as.polygons(terra::ext(df), crs = terra::crs(df)))
    )
    
  } else if (inherits(df, c("stars", "sf"))) {
    bbox <- sf::st_bbox(df)
    
  } else {
    stop("Unsupported spatial data class.", call. = FALSE)
  }
  
  return(bbox)
}

# get coastline data -------------------------------------------------
# Helper function to get coastline data based on region
get_coastline_data <- function(region, data_bbox, resolution = "medium") {
  
  # Define region bounding boxes
  region_boxes <- list(
    shape = c(data_bbox$xmin, data_bbox$xmax, data_bbox$ymin, data_bbox$ymax),
    nw_atlantic = c(xmin = -68, xmax = -54, ymin = 40, ymax = 48),
    pacific = c(xmin = -180, xmax = -100, ymin = 30, ymax = 65),
    gsl = c(xmin = -72, xmax = -55, ymin = 45, ymax = 52),
    arctic = c(xmin = -180, xmax = -40, ymin = 65, ymax = 85)
  )
  
  # Determine bounding box
  if (is.character(region) && region %in% names(region_boxes)) {
    bbox <- region_boxes[[region]]
  } else if (is.numeric(region) && length(region) == 4) {
    bbox <- region
    names(bbox) <- c("xmin", "xmax", "ymin", "ymax")
  } else {
    # Default to data extent with buffer
    bbox <- as.numeric(data_bbox)
    bbox[1] <- bbox[1] - 5  # xmin
    bbox[2] <- bbox[2] - 5  # ymin
    bbox[3] <- bbox[3] + 5  # xmax
    bbox[4] <- bbox[4] + 5  # ymax
    names(bbox) <- c("xmin", "ymin", "xmax", "ymax")
    bbox <- bbox[c("xmin", "xmax", "ymin", "ymax")]  # Reorder
  }
  # Ensure bbox values are valid
  bbox[1] <- max(bbox[1], -180)  # xmin
  bbox[2] <- min(bbox[2], 180)   # xmax
  bbox[3] <- max(bbox[3], -90)   # ymin
  bbox[4] <- min(bbox[4], 90)    # ymax
  
  
  # Try different data sources
  coastline_data <- NULL
  
  # Option 1: Try rnaturalearth with better error handling
  if (requireNamespace("rnaturalearth", quietly = TRUE)) {
    scale_map <- switch(resolution,
                        "low" = 110,
                        "medium" = 50,
                        "high" = 10,
                        50)
    
    tryCatch({
      # Try to get countries data first
      world <- rnaturalearth::ne_countries(scale = scale_map, returnclass = "sf")
      
      # Check if data is valid
      if (!is.null(world) && nrow(world) > 0) {
        # Create bbox polygon correctly using st_polygon
        bbox_coords <- matrix(c(
          bbox[1], bbox[3],  # xmin, ymin
          bbox[2], bbox[3],  # xmax, ymin
          bbox[2], bbox[4],  # xmax, ymax
          bbox[1], bbox[4],  # xmin, ymax
          bbox[1], bbox[3]   # close polygon
        ), ncol = 2, byrow = TRUE)
        
        bbox_poly <- sf::st_sfc(sf::st_polygon(list(bbox_coords)), crs = 4326)
        
        # Crop to region
        coastline_data <- sf::st_crop(world, bbox_poly)
        
        # Check if result is valid
        if (is.null(coastline_data) || nrow(coastline_data) == 0) {
          coastline_data <- NULL
        }
      }
    }, error = function(e) {
      message("rnaturalearth failed: ", e$message)
      coastline_data <- NULL
    })
  }
  
  # Option 2: Try maps package
  if (is.null(coastline_data) && requireNamespace("maps", quietly = TRUE)) {
    tryCatch({
      world_map <- maps::map("world",
                             xlim = bbox[c(1,2)],
                             ylim = bbox[c(3,4)],
                             plot = FALSE, fill = TRUE)
      
      coastline_data <- sf::st_as_sf(world_map)
      coastline_data <- st_transform(coastline_data, crs = crs(st_as_sf(bbox_poly)))
      coastline_data <- sf::st_crop(coastline_data, bbox_poly)
      
    }, error = function(e) {
      warning("Could not retrieve coastline data from maps package: ", e$message, call. = FALSE)
    })
  }
  
  # Option 3: Create basic outline based on region
  if (is.null(coastline_data)) {
    warning("No coastline packages available. Install 'rnaturalearth' or 'maps' for coastline data.",
            call. = FALSE)
    
    # Create basic regional outlines
    coastline_data <- create_basic_coastline(region, bbox)
  }
  
  return(coastline_data)
}

# Helper function to create basic coastline outlines
create_basic_coastline <- function(region, bbox) {
  if (is.character(region)) {
    coords <- switch(region,
                     nw_atlantic = data.frame(
                       x = c(-80, -60, -40, -40, -60, -80, -80),
                       y = c(30, 30, 40, 70, 70, 50, 30)
                     ),
                     pacific = data.frame(
                       x = c(-180, -130, -100, -100, -130, -180, -180),
                       y = c(30, 30, 35, 65, 65, 50, 30)
                     ),
                     gsl = data.frame(
                       x = c(-72, -55, -55, -72, -72),
                       y = c(45, 45, 52, 52, 45)
                     ),
                     arctic = data.frame(
                       x = c(-180, -40, -40, -180, -180),
                       y = c(65, 65, 85, 85, 65)
                     ),
                     # Default rectangle
                     data.frame(
                       x = c(bbox[1], bbox[2], bbox[2], bbox[1], bbox[1]),
                       y = c(bbox[3], bbox[3], bbox[4], bbox[4], bbox[3])
                     )
    )
  } else {
    # Use provided bbox
    coords <- data.frame(
      x = c(bbox[1], bbox[2], bbox[2], bbox[1], bbox[1]),
      y = c(bbox[3], bbox[3], bbox[4], bbox[4], bbox[3])
    )
  }
  
  sf::st_sf(
    id = 1,
    geometry = sf::st_sfc(sf::st_polygon(list(as.matrix(coords))), crs = 4326)
  )
}
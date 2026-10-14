#' Summary method for an ea_data or ea_spatial object
#' @param x an ea_data or ea_spatial object.
#' @param ... further arguments passed to summary method
#' @author Emily O'Grady
#' @export
#' 
ea.summary <- function(x, ...) {
 if (inherits(x, 'ea_data')) {

    # Basic summary statistics for all columns with "_value"
   value_cols <- grep("_value$", names(x@data), value = TRUE)
   summary_stats <- list()
   if (length(value_cols) > 1) {
     summary_stats <- lapply(value_cols, function(col) summary(x@data[[col]]))
     names(summary_stats) <- value_cols
   } else {
     summary_stats <- summary(x@data[[value_cols]])
   }


    cat("--- Summary of ea_data ---\n")
    cat("Metadata:\n")
    cat("  Data Type: ", x@meta$data_type, "\n")
    if (!is.na(x@meta$species) && !is.null(x@meta$species) && x@meta$species != "") {
      cat("  Species: ", x@meta$species, "\n")
    }
    cat("  Location:  ", x@meta$location_descriptor, "\n")
    cat("  Region:  ", x@meta$region, "\n")
    cat("  Source:  ", x@meta$source_citation, "\n\n")

    cat("Data Overview:\n")
    cat("  Time range: ", min(x@data$year, na.rm = TRUE), " to ", max(x@data$year, na.rm = TRUE), "\n")
    cat("  Number of observations: ", nrow(x@data), "\n\n")

    cat("Summary of 'value' column (Units: ", x@meta$units, "):\n", sep = "")
    print(summary_stats)

    invisible(list(meta = x@meta, value_summary = summary_stats))
 } else if (inherits(x, 'ea_spatial')) {
  
     cat("--- Summary of ea_spatial Object ---\n")
     cat("Metadata:\n")
     cat("  Data Type: ", x@meta$data_type, "\n")
     cat("  Region:    ", x@meta$region, "\n")
     cat("  Time:      ", x@meta$time_descriptor, "\n")
     cat("  Source:    ", x@meta$source_citation, "\n\n")

     
     if(inherits(x@data, "sf")){
       cat("Spatial Information (from sf):\n")
       # Print the bounding box and CRS from the underlying sf object
       print(sf::st_geometry(x@data))
       cat("\n")
     } else if(inherits(x@data, "stars")){
       cat("Spatial Summary (from stars):\n")
       # Print the bounding box and CRS from the underlying sf object
       print(summary(x@data))
       print(paste0("CRS: ",sf::st_crs(x@data)$input))
       print(sf::st_bbox(x@data))
       print(stars::st_dimensions(x@data))
       cat("\n")
     }

     # If there's a time_descriptor column, show unique values
     if ("time_descriptor" %in% names(x@data)) {
       unique_times <- unique(x@data$time_descriptor)
       cat("Time periods: ", paste(unique_times, collapse = ", "), "\n")
       cat("Number of time periods: ", length(unique_times), "\n\n")
     }

     cat("Summary of 'value' column (Units: ", x@meta$units, "):\n", sep = "")
     print(summary(x@data$value))

     invisible(list(meta = x@meta, value_summary = summary(x@data$value)))
   
 } else{
   stop('Unknown class for summary: ', class(x), '. Please provide an ea_data or ea_spatial object.')
 }
}

library(reticulate)
library(dplyr)
library(stars)
library(sf)
library(tidyr)
library(lubridate)
library(stringr)

username <- Sys.getenv("CMEMS_USERNAME")
password <- Sys.getenv("CMEMS_PASSWORD")
my_ds    <- "cmems_mod_glo_phy_my_0.083deg_P1M-m"

lonmin <- -67.74250; lonmax <- -54.90132
latmin <- 40.04343;  latmax <- 47.83333

start10 <- Sys.Date() - years(10)
end10   <- Sys.Date()

# create a virtual environment
virtualenv_create("CopernicusMarine")

# install the copernicus python package
virtualenv_install("CopernicusMarine", packages = "copernicusmarine")

reticulate::use_virtualenv("CopernicusMarine", required = TRUE)
cmt <- reticulate::import("copernicusmarine")
cmt$login(username = username, password = password)
all_long <- list()
# Create temporary file for download
temp_nc1 <- tempfile(fileext = ".nc")

# Download data 
cmt$subset(
  dataset_id = "cmems_mod_glo_phy_my_0.083deg_P1M-m",
  variables = list("bottomT"),
  minimum_longitude = -67.74250,
  maximum_longitude = -54.90132,
  minimum_latitude = 40.04343,
  maximum_latitude = 47.83333,
  start_datetime = paste0(start10, "T00:00:00"),
  end_datetime = paste0(end10,   "T23:59:59"),
  output_directory = dirname(temp_nc1),
  output_filename = basename(temp_nc1)
)


clean_glorys_nc <- function(nc_file, var_name = "bottomT", digits = 6) {
  
  # Read and process the netCDF file as stars
  full <- stars::read_ncdf(nc_file, var = var_name)
  full_crs <- st_crs(full)
  
  full <- full %>%
    round(digits = digits)
  
  # ensure consistent resolution
  lon <- st_get_dimension_values(full, "longitude")
  lat <- st_get_dimension_values(full, "latitude")
  
  dx <- mean(diff(lon))
  dy <- mean(diff(lat))
  
  full <- st_set_dimensions(
    full, "longitude",
    values = NULL,
    offset = lon[1] - dx / 2,
    delta = dx
  )
  
  full <- st_set_dimensions(
    full, "latitude",
    values = NULL,
    offset = lat[1] - dy / 2,
    delta = dy
  )
  
  # set original crs
  full <- full %>% st_set_crs(full_crs)
  
  # return
  return(full)
}

df1 <- clean_glorys_nc(temp_nc1, var_name = "bottomT")


# convert to ea_spatial
glorys_bottom_temperature <- as_ea_spatial(
  x = df1,
  value_col  =  "bottomT",
  data_type    = "Bottom Temperature",
  units        = "degC",
  region       = "Northwest Atlantic",
  time_descriptor = sprintf("Monthly data %s→%s", 
                            str_sub(
                              first(stars::st_get_dimension_values(df1,"time")),
                              1,7), 
                            str_sub(
                              last(stars::st_get_dimension_values(df1,"time")),
                              1,7)
                            ),  
  source_citation = "CMEMS Global Ocean Physics Reanalysis",
  temporal_coverage = sprintf("%s to %s", 
                              str_sub(
                                first(stars::st_get_dimension_values(df1,"time")),
                                1,7), 
                              str_sub(
                                last(stars::st_get_dimension_values(df1,"time")),
                                1,7)),
  variable_name = "bottomT",
  dataset_id   = paste(my_ds)
)


# test conversion to spatraster
#test1 <- terra::rast(glorys_bottom_temperature@data["value"])
#plot(test1$lyr.1)

usethis::use_data(glorys_bottom_temperature, overwrite=TRUE)



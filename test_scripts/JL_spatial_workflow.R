library(marea)
library(stars)
library(terra)
library(sf)

# Your prepared stars object
data("glorys_bottom_temperature")

# subset to only July temperature
glorys_bottom_temperature <- ea.subset.spatial.dims(glorys_bottom_temperature, months = 7)

plot(glorys_bottom_temperature, style = "fill")
JL(satellite_ocean_colour, style = "fill", coastline_region = "shape")

stars_obj <- ea.subset.spatial.dims(glorys_bottom_temperature,
                                    years = seq(2016, 2026, by = 2),
                                    month = 7)
                        
target_var <- "bottomT_value"       
target_var_short <- stringr::str_remove(target_var, "_value")
stars_obj <- stars_obj[target_var]
star <- stars_obj@data
names(star) <- target_var_short

# Convert to terra
rast <- terra::rast(star)
terra::time(rast) <- stars::st_get_dimension_values(star, "time")
names(rast) <- paste0(terra::time(rast))

# Convert to sf points (subset time first to limit size)
star_small <- star[, , , ]
pts <- sf::st_as_sf(
  star_small,
  as_points = TRUE,
  long = TRUE,
  na.rm = TRUE
)

# Shared metadata
meta <- list(
  data_type = "Bottom temperature - July",
  region = "Northwest Atlantic",
  time_descriptor = "annual",
  units = "degC"
)

# Test the existing constructor
ea_star <- do.call(
  marea::ea_spatial,
  c(list(data = star, value_col = target_var_short), meta)
)

ea_rast <- do.call(
  marea::ea_spatial,
  c(list(data = rast, value_col = names(rast)), meta)
)

ea_sf <- do.call(
  marea::ea_spatial,
  c(list(data = pts, value_col = target_var_short), meta)
)

# Inspect results
names(ea_star@data)
names(ea_rast@data)
names(ea_sf@data)


methods::validObject(ea_star)
methods::validObject(ea_rast)
methods::validObject(ea_sf)

stars::st_get_dimension_values(ea_star@data, "time")
terra::time(ea_rast@data)
unique(ea_sf@data$time)



# test ea plot for stars:
JL(ea_star, style = "fill") + 
  facet_wrap(~time)

JL(ea_star, 
   style = "contour", 
   contour.bw = 5, 
   coastline_region = "shape") + 
  facet_wrap(~time)

JL(ea_star, 
   style = "contour", 
   contour.type = "aggregate", 
   contour.bw = 2,
   coastline_region = "shape")

ea_star2 <- ea.subset.spatial.values(ea_star, 
                                     variable = target_var,
                                     value_range = c(8,Inf))
JL(ea_star2, style = "area") +
  facet_wrap(~time)


# test ea plot for spatraster:
JL(ea_rast, style = "fill") +
  facet_wrap(~lyr)

JL(ea_rast, 
   style = "contour", 
   contour.bw = 2,
   coastline_region = "shape") + 
  facet_wrap(~lyr)

JL(ea_rast, 
   style = "contour", 
   contour.type = "aggregate", 
   contour.bw = 2,
   coastline_region = "shape") 

ea_rast2 <- ea.subset.spatial.values(ea_rast,
                                     value_range = c(10,Inf))
JL(ea_rast2, style = "area") + 
  facet_wrap(~lyr)



# test ea plot for sf
JL(ea_sf, style = "fill", point_size = .1) + 
  facet_wrap(~time)

JL(ea_sf, style = "contour")


ea_sf2 <- ea.subset.spatial.values(ea_sf, 
                                   target_var, 
                                   value_range = c(0,90))
JL(ea_sf2, style = 'area', point_size = .1) +
  facet_wrap(~time)

JL(ea_sf2, style = 'bubble')




# subset to shape ---------------------------------------------------------

library(Mar.data) # available from https://github.com/PopulationEcologyDivision/Mar.data
library(sf)
data("LFAs_sf")

lfa_33 <- LFAs_sf %>%
  filter(LFA == "33") %>%
  st_transform(crs = st_crs(satellite_ocean_colour@data)) # use same CRS as the data for spatial operations


ea_star_lfa <- ea.subset.spatial.dims(ea_star, shape = lfa_33)
JL(ea_star_lfa, 
     style = "fill",
   coastline_region = "nw_atlantic") + 
  geom_sf(data = lfa_33,
          color = "red", fill = "transparent")



ea_rast_lfa <- ea.subset.spatial.dims(ea_rast, shape = lfa_33)
JL(ea_rast_lfa, 
   style = "fill",
   coastline_region = "nw_atlantic") + 
  geom_sf(data = lfa_33,
          color = "red", fill = "transparent") + 
  facet_wrap(~lyr)

ea_sf_lfa <- ea.subset.spatial.dims(ea_sf, shape = lfa_33)
JL(ea_sf_lfa, 
   style = "fill",
   point_size = .9) + 
  geom_sf(data = lfa_33,
          color = "red", fill = "transparent") + 
  facet_wrap(~time)
  



devtools::load_all()
library(dplyr)
library(readr)
library(tibble)
library(marea)

# Helper function
source("data-raw/00-data-raw-helper.R")

remotes::install_github("casaultb/azmpdata")
library(azmpdata)


# ---- AZMP BOTTOM TEMPERATURE ----
azmp_bottom_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$temperature_at_sea_floor
) %>%
  filter(!is.na(mean))

azmp_bottom_temperature <- make_ea_index(
  df = azmp_bottom_temperature_new,
  value_col = "mean",
  data_type = "Bottom Temperature",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea floor mean temperature",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Galbraith, P.S., Lizotte, M., Blais, M., Bélanger, D., Casault, B., Coyne, J., Layton, C., Azetsu-Scott, K., Beazley, L., Chassé, J., Clay, S., Cyr, F., Devred, E., Fudge, A., Gabriel, C.-E., Greenan, B., Hébert, A.-J., Johnson, C.L., Maillet, G., Penney, J., Rastin, S., Ringuette, M., Shaw, J.-L., Snook, S., Starr, M. 2025. Oceanographic conditions in the Atlantic zone in 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 400 : viii + 49 p. https://doi.org/10.60825/e92v-d229"
)

usethis::use_data(azmp_bottom_temperature, overwrite = TRUE)

# ---- AZMP SURFACE TEMPERATURE ----
azmp_surface_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$sea_temperature_0
) %>%
  filter(!is.na(mean))

azmp_surface_temperature <- make_ea_index(
  df = azmp_surface_temperature_new,
  value_col = "mean",
  data_type = "Surface Temperature",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean temperature",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Galbraith, P.S., Lizotte, M., Blais, M., Bélanger, D., Casault, B., Coyne, J., Layton, C., Azetsu-Scott, K., Beazley, L., Chassé, J., Clay, S., Cyr, F., Devred, E., Fudge, A., Gabriel, C.-E., Greenan, B., Hébert, A.-J., Johnson, C.L., Maillet, G., Penney, J., Rastin, S., Ringuette, M., Shaw, J.-L., Snook, S., Starr, M. 2025. Oceanographic conditions in the Atlantic zone in 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 400 : viii + 49 p. https://doi.org/10.60825/e92v-d229"
)

usethis::use_data(azmp_surface_temperature, overwrite = TRUE)

# ---- AZMP SURFACE TEMPERATURE FROM SATELLITE ----
azmp_satellite_temperature_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$sea_surface_temperature_from_satellite
) %>%
  filter(!is.na(mean))

azmp_satellite_temperature <- make_ea_index(
  df = azmp_satellite_temperature_new,
  value_col = "mean",
  data_type = "Surface Temperature from Satellite",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean temperature from satellite",
  units = "deg C",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Galbraith, P.S., Lizotte, M., Blais, M., Bélanger, D., Casault, B., Coyne, J., Layton, C., Azetsu-Scott, K., Beazley, L., Chassé, J., Clay, S., Cyr, F., Devred, E., Fudge, A., Gabriel, C.-E., Greenan, B., Hébert, A.-J., Johnson, C.L., Maillet, G., Penney, J., Rastin, S., Ringuette, M., Shaw, J.-L., Snook, S., Starr, M. 2025. Oceanographic conditions in the Atlantic zone in 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 400 : viii + 49 p. https://doi.org/10.60825/e92v-d229"
)

usethis::use_data(azmp_satellite_temperature, overwrite = TRUE)

# ---- AZMP STRATIFICATION ----
azmp_stratification_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$density_gradient_0_50
) %>%
  filter(!is.na(mean))

azmp_stratification <- make_ea_index(
  df = azmp_stratification_new,
  value_col = "mean",
  data_type = "Stratification Index",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO stratification index",
  units = "kg/m^3 per meter",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Galbraith, P.S., Lizotte, M., Blais, M., Bélanger, D., Casault, B., Coyne, J., Layton, C., Azetsu-Scott, K., Beazley, L., Chassé, J., Clay, S., Cyr, F., Devred, E., Fudge, A., Gabriel, C.-E., Greenan, B., Hébert, A.-J., Johnson, C.L., Maillet, G., Penney, J., Rastin, S., Ringuette, M., Shaw, J.-L., Snook, S., Starr, M. 2025. Oceanographic conditions in the Atlantic zone in 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 400 : viii + 49 p. https://doi.org/10.60825/e92v-d229",
  details = "An index of stratification, measured as the density difference between 0 and 50 metres"
)

usethis::use_data(azmp_stratification, overwrite = TRUE)

# ---- AZMP SALINITY ----
azmp_salinity_new <- tibble(
  year   = azmpdata::Derived_Annual_Broadscale$year,
  region = azmpdata::Derived_Annual_Broadscale$area,
  mean   = azmpdata::Derived_Annual_Broadscale$salinity_0
) %>%
  filter(!is.na(mean))

azmp_salinity <- make_ea_index(
  df = azmp_salinity_new,
  value_col = "mean",
  data_type = "Surface Salinity",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "NAFO sea surface mean salinity",
  units = "PSU",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  citation = "Galbraith, P.S., Lizotte, M., Blais, M., Bélanger, D., Casault, B., Coyne, J., Layton, C., Azetsu-Scott, K., Beazley, L., Chassé, J., Clay, S., Cyr, F., Devred, E., Fudge, A., Gabriel, C.-E., Greenan, B., Hébert, A.-J., Johnson, C.L., Maillet, G., Penney, J., Rastin, S., Ringuette, M., Shaw, J.-L., Snook, S., Starr, M. 2025. Oceanographic conditions in the Atlantic zone in 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 400 : viii + 49 p. https://doi.org/10.60825/e92v-d229"
)

usethis::use_data(azmp_salinity, overwrite = TRUE)
  

# ---- AZMP Carbonate ----
carbonate_new <- tibble(
  year   = azmpdata::Derived_Annual_Carbonate$year,
  section = azmpdata::Derived_Annual_Carbonate$section_name,
  depth = azmpdata::Derived_Annual_Carbonate$depth_m,
  TA_mean_umolkg   = azmpdata::Derived_Annual_Carbonate$mean_TA_umolkg,
  DIC_mean_umolkg  = azmpdata::Derived_Annual_Carbonate$mean_DIC_umolkg,
  pH_mean         = azmpdata::Derived_Annual_Carbonate$mean_pH_total,
  pco2_mean_uatm   = azmpdata::Derived_Annual_Carbonate$mean_pCO2_uatm,
  substrate_inhibitor_ratio = azmpdata::Derived_Annual_Carbonate$mean_substrate_inhibitor_ratio,
  omega_aragonite_mean = azmpdata::Derived_Annual_Carbonate$mean_omega_aragonite,
  omega_calcite_mean = azmpdata::Derived_Annual_Carbonate$mean_omega_calcite,
  carbonate_system_vulnerability_index = azmpdata::Derived_Annual_Carbonate$mean_carbonate_system_vulnerability_index
) 

# change all -999 to NA
carbonate_new[carbonate_new< (-998)] <- NA



azmp_carbonate <- make_ea_index(
  df = carbonate_new,
  value_col = c("TA_mean_umolkg", 
                "DIC_mean_umolkg", 
                "pH_mean", 
                "pco2_mean_uatm",
                "substrate_inhibitor_ratio",
                "omega_aragonite_mean",
                "omega_calcite_mean",
                "carbonate_system_vulnerability_index"),
  data_type = "Synthesized Carbonate Indices",
  region = "Scotian Shelf (4X, 4V, 4W)",
  location = "Maritimes region",
  units = "TA = umol/kg, DIC = umol/kg, pH = total scale, pCO2 = uatm, substrate-inhibitor ratio = unitless, omega aragonite = unitless, omega calcite = unitless, carbonate system vulnerability index = unitless",
  source = "DFO Atlantic Zone Monitoring Program via azmpdata package (https://casaultb.github.io/azmpdata/)",
  principal_investigator = "Kumiko Azetsu-Scott (Kumiko.Azetsu-Scott@dfo-mpo.gc.ca)",
  citation = "Lizotte, M., Blais, M., Chassé, J., Galbraith, P. S., Hébert, A.-J., Starr, M. 2026. Acidification and CO2-Driven Conditions in the Estuary and Gulf of St. Lawrence During 2024. Can. Tech. Rep. Hydrogr. Ocean. Sci. 410 : vi + 71 p."
)

usethis::use_data(azmp_carbonate, overwrite = TRUE)

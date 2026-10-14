# Helper function for converting raw data into EA index
make_ea_index <- function(
    df, value_col, data_type, region, location, units, source, species = NA_character_, ...
) {
  ea_data(
    data = df,
    value_col = value_col,
    data_type = data_type,
    region = region,
    location_descriptor = location,
    units = units,
    species = species,
    source_citation = source, 
    ...
  )
}

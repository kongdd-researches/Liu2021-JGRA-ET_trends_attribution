file_continent <- "I:/GitHub/shapefiles/continent.shp"

get_continent <- function() {
    read_sf(file_continent) %>% as_Spatial()
}

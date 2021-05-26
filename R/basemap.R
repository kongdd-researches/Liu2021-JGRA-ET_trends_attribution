file_continent <- "I:/Research/phenology/phenologyTP/data-raw/shp/continent.shp"

get_continent <- function() {
    read_sf(file_continent) %>% as_Spatial()
}

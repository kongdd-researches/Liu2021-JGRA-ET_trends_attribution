{
    library(sf)
    shp_cont <- read_sf("I:/Research/phenology/phenologyTP/data-raw/shp/continent.shp") %>%
        st_simplify(dTolerance = 0.02) %>%
        as_Spatial()

    sp_cont <- list("sp.lines", shp_cont, lwd = 0.5, first = F)
    # sp_sign = list("sp.polygons", poly_shade, first = FALSE, lwd = 0.1)
    # sp_sign,
    sp_layout <- list(sp_cont)
}

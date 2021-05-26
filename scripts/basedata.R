{
    library(sf)
    st_cont <- read_sf("I:/Research/phenology/phenologyTP/data-raw/shp/continent.shp") %>%
        st_simplify(dTolerance = 0.02)
    shp_cont = as_Spatial(st_cont)

    sp_cont <- list("sp.lines", shp_cont, lwd = 0.5, first = F)
    # sp_sign = list("sp.polygons", poly_shade, first = FALSE, lwd = 0.1)
    # sp_sign,
    sp_layout <- list(sp_cont)
}

geom_global = st_cont %>% st_union()
st = st_cont %>% rbind(
    st_sf(data.frame(CONTINENT="global", geom=geom_global))
)

bands <- c("OBS", "ALL", "GHG", "AER", "NAT")
files <- c("trend_GCMs-E.tif", "trend_GCMs-Et.tif", "trend_GCMs-Ev.tif") %>%
    set_names(c("ET", "Et", "Ev"))

# r = overlap(raster(files[1]), st), future tasks
r_mean <- extract2(files, st) %>% tidy_zonal
r_sd   <- extract2(files, st, sf_sd) %>% tidy_zonal

tidy_zonal <- function(r) {
    map(r, ~set_colnames(.x, bands) %>% cbind(region = st$CONTINENT)) %>%
        melt_list("band") %>%
        data.table() %>%
        .[region != "Antarctica"] %>%
        dt_round(3)
}

d_trend <- listk(mean = r_mean, sd = r_sd) %>% melt_list("type") %>%
    melt(c("type", "band", "region")) %>%
    dcast2("type") %>% mutate(str = paste0(mean, " ± ", sd)) %>%
    dcast(band + region ~ variable, value.var = "str")
#
write_list2xlsx(list(d_trend = d), "tbl0_trend_regionMean.xlsx")
file.show("tbl0_trend_regionMean.xlsx")

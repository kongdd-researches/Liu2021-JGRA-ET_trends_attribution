# !/usr/bin/Rscript
# Dongdong Kong ----------------------------------------------------------------
source("scripts/main_pkgs.R")

{
  library(lattice.layers)
  library(sf)
  library(sf2)
  sp_cont <- list("sp.lines", get_continent(), lwd = 0.2, first = F)
  sp_layout = list(sp_cont)
  # sp_sign = list("sp.polygons", poly_shade, first = FALSE, lwd = 0.1)
  # sp_sign,
}
load(file = "data-raw/Multimodel mean E Trends from CIMP6 and GLEAM.RData")

AI = raster("INPUTS/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif") %>%
  raster2array() %>% as.numeric()

# grid <- make_grid()
# data = data.frame(E = Trends_ALL$E[, 1], AI)
# grid@data <- data
# sp_plot(grid) + layer_title()
# image(r)
# AI <- readGDAL("INPUTS/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif")
# AI$E = Trends_ALL$E[, 1]

# prepare plot data
{
  set_options(list(style = "EN"))
  grid <- expand.grid(s1 = seq(-179.5, 179.5, 1), s2 = seq(-89.5, 89.5, 1)) %>%
    cbind(I = 1:nrow(.), .) %>%
    df2sp(formula = ~ s1 + s2) %>%
    sf2::as_SpatialPixelsDataFrame()
  # grid2 <- melt(grid, id.vars = c('lon','lat'), variable.name = 'Factor', value.name = 'trends')
  df2 <- Trends_ALL %>%
    map(~ cbind(I = 1:nrow(.), ., AI)) %>%
    melt_tree(c("band")) %>%
    melt(c("band", "I", "AI"), variable.name = "forcing") %>%
    data.table()
  df2$forcing %<>% recode(Obs = "OBS")
}

{
  brks = c(-Inf, 0.05, 0.2, 0.5, 0.65, Inf)
  levs = c("hyper-arid", "arid", "semi-arid", "sub-humid", "humid")

  brks = c(-Inf, 0.2, 0.5, 0.65, Inf)
  levs = c("arid", "semi-arid", "sub-humid", "humid")

  df2 %<>% mutate(AI2 = cut(AI, brks, levs),
                  sign = sign(value))
}
d = df2[!is.na(value + AI)]
info = df2[!(is.na(AI2) | is.na(sign)), .N, .(band, AI2, forcing, sign)]
info[, perc := round(N/sum(N)*100, 1), .(band, forcing)] %>%
  select(-N) %>%
  dcast2("AI2", "perc")


ggplot(info, aes(AI2, N, fill = as.factor(sign))) +
  geom_histogram(stat = "identity")

{
  theme_set(
    theme_grey()
  )
  p <- ggplot(d,
              aes(AI2, value, fill = AI2)) +
    stat_summary(fun.data = box_qtl, geom = "errorbar", width = 0.5) +
    geom_boxplot2(notch = TRUE, outlier.shape = NA, coef = 0, width = 0.8) +
    # geom_boxplot2() +
    coord_cartesian(ylim = c(-1.5, 1.5)) +
    facet_grid(band~forcing, scales = "free") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  write_fig(p, "a.pdf", 10, 7)
}
d[band == "E" & forcing == "OBS", .N, .(AI2)] %>%
  mutate(perc = round(N/sum(N)*100, 1))

{
  par(mar = c(3, 2, 2, 1), mgp = c(3, 0.6, 0))
  p <- xyplot(value ~ AI | band + forcing, d,
              # [forcing == "OBS"],
              xlim = c(0, 3), ylim = c(-1, 1),
              # scales = list(cex = 1.4),
              mgp = c(3, 0.6, 0),
              par.strip.text = list(font = 2, fontfamily = "Times", lineheight = 2),
              panel = function(x, y, ...) {
                # browser()
                slp = slope_p(y, x)
                label = sprintf("slope = %.2f, p = %.3f", slp[1], slp[2])
                panel.smoothScatter(x, y, ...)
                panel.lmline(x, y, ...)
                # panel.text(-Inf, Inf, label, adj = c(0, 1), cex = 2)
                # panel.text(1, 1, label, adj = c(0, 1), cex = 2)
                grid.text(label, 0.01, 0.99, hjust = 0, vjust = 1)
              })
  p <- latticeExtra::useOuterStrips(p)
    # theme_lattice(
    #   plot.margin = c(0, 0, 0, 0),
    #   axis.margin = c(0, 0, 0, 0),
    #   axis.components.outer = c(0, 0, 0, 0),
    #   axis.components.inner = c(0, 0, 0, 0)
    # )
  write_fig(p, "a.pdf", 8, 10)
}
thematic_off()
# thematic_on(font = font_spec("Times", 1.2))


# smoothScatter(d$AI, d$value)
{
  p <- ggplot(d) + aes(AI, value) +
    # scale_x_log10() + scale_y_log10() +
    stat_density2d(geom="tile", aes(fill=..density..^0.25, alpha=1), contour=FALSE) +
    geom_point(size=0.5) +
    stat_density2d(geom="tile", aes(fill=..density..^0.25,     alpha=ifelse(..density..^0.25<0.4,0,1)), contour=FALSE) +
    scale_fill_gradientn(colours = colorRampPalette(c("white", blues9))(256))
  # p <- ggplot(d, aes(AI, value)) +
  #   # geom_density2d() +
  #   stat_density2d(aes(fill = ..density..^0.25), geom = "tile", contour = FALSE, n = 200) +
  #   # geom_point() +
  #   # geom_smooth() +
  #   facet_grid(forcing~band, scales = "free") +
  #   coord_cartesian(xlim = c(0, 3))
  write_fig(p, "a.pdf")
}

# library(rgdal)
# foreach(d = Trends_ALL, name = names(Trends_ALL)) %do% {
#   grid@data <- d
#   outfile = glue::glue("trend_GCMs-{name}.tif")
#   writeGDAL(grid, outfile)
# }
# extract2("trend_GCMs-E.tif", )

# Tables
{
  d_obs = df2[forcing == "OBS"] %>% dcast2(by = "band", value.var = "value") %>% data.table() %>%
    .[!is.na(E), ]
  d_cont = d_obs[, abs(.SD/E)*100, .SDcols = c("Et", "Ev")] %T>% summary()

  bands = c("E", "Et", "Ev") %>% set_names(., .)
  bandName = "Et"
  d_agree = foreach(bandName = bands) %do% {
    d_E = df2[band == bandName] %>% dcast2(by = "forcing", value.var = "value") %>% data.table() %>%
      .[!is.na(OBS) & OBS != 0, ]
    # tbl = d_E[, -(1:2)] %>% sign_agree()
    melt(d_E, c("band", "I", "OBS")) %>% data.table() %>%
      .[, sign_agree(OBS, value) %>% as.list, .(variable)]
  } %>% melt_list("band")
  tbl_1 = d_agree %>% dcast(variable ~ band)
  # variable         E         Et        Ev
  # 1      ALL 0.6790188 0.62286598 0.6637030
  # 2      GHG 0.6773963 0.09295604 0.6696736
  # 3      AER 0.5524044 0.13980374 0.5681095
  # 4      NAT 0.5725226 0.18174486 0.5361802
}

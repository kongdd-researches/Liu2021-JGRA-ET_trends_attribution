# !/usr/bin/Rscript
# Dongdong Kong ----------------------------------------------------------------
source("scripts/main_pkgs.R")
load_all()
{
  library(lattice.layers)
  library(sf)
  library(sf2)
  sp_cont <- list("sp.lines", get_continent(), lwd = 0.2, first = F)
  sp_layout = list(sp_cont)
}

load(file = "data-raw/Multimodel mean E Trends from CIMP6 and GLEAM.RData")
AI = raster("INPUTS/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif") %>%
  raster2array() %>% as.numeric()

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

## 2. visualization ------------------------------------------------------------
# parameters for plot
{
  brks <- c(0, 0.2, 0.5, 1, 1.5, 2) %>% { c(-Inf, -rev(.), 0, ., Inf) } %>% unique()
  nbrk <- length(brks) - 1
  cols <- get_color(rcolors$amwg256, nbrk)
  mid <- nbrk / 2 + c(0, 1)
  cols[mid] <- "grey"
  pars <- list(title = list(x = -170, y = 92, cex = 1.4, adj = c(0, 1)))
}

## 2.2 visualization ------------------------------------------------------------
# df2$band %<>% mapvalues(c("E", "Et", "Ev"))
{
  devtools::load_all("I:/GitHub/rpkgs/lattice.layers.R")
  pars <- list(title = list(x = -170, y = 100, cex = 1.4, adj = c(0, 1)))
  p <- sp_plot(grid,
               df2[band == "E"],
               # df2[band == "E" & forcing == "OBS"],
               # layout = c(2, 3),
               formula = value ~ lon + lat |  forcing,
               aspect = 0.45,
               pars = pars, unit = "mm/y",
               ylim = c(-60, 92), xlim = c(-180, 240),
               sp.layout = sp_layout,
               par.strip.text = list(cex = 1.5, font = 2, fontfamily = "TimesSimSun", lineheight = 2),
               # par.settings2 = list(axis.line = list(col = "black")),
               key.num2factor = TRUE,
               colorkey = list(space = "right", height = 0.985),
               brks = brks, colors = cols
  ) +
    layer_statistic(x = 0.4, y = 0.1, cex = 1.4, fill = "white") +
    layer_barchart(x = 0.08, y = 0.05, height = 0.24, width = 0.18, title = FALSE, xlabels = FALSE) +
    layer_latFreq(bbox =c(190, 240, -60, 92), unit = "native",
                  is_spatial = TRUE,
                  ylabels = FALSE,
                  col.regions = c("red", "blue"),
                  zlim = c(-1, 1),
                  cex = 1.2,
                  ylim = c(-60, 90)) +
    layer_title(x = 0.01, y = 0.995, labels = NULL) +
    layer_signPerc(x = 0.01, y = 0.58) +
    theme_lattice(
      key.margin = c(0, 1.5, 0, 0),
      plot.margin = c(0, 5.5, 0.5, 1)
    )
  outfile = glue::glue("Figure1_all_spatial trends of signals.pdf")
  write_fig(p, outfile, 12, 8, devices = "pdf")
  # write_fig(p2, outfile, 14, 10, devices = "jpg")
}


{
    write_fig({
        # grid.newpage()
        x <- stats::runif(20)
        y <- stats::runif(20)
        rot <- stats::runif(20, 0, 360)
        grid.rect(gp = gpar(fill = "red"))
        grid.text("SOMETHING NICE AND BIG", x=x, y=y, rot=rot,
                  gp=gpar(fontsize=20, col="grey"))

        # title <- grobTree( rectGrob(gp=gpar(fill="black")),
        #                    textGrob("Testing title background",
        #                             gp=gpar(fontsize=15, col="white", fontface="bold")))
        # grid.draw(title)
    })
}

# bands = c("E", "Et", "Ev") %>% set_names(., .)
# for(bandName in bands[1:1]) {
#   load_all("I:/Research/phenology/lattice.layers.R")
#   p <- sp_plot(grid,
#     # df2,
#     df2[band == bandName],
#     formula = value ~ lon + lat | forcing, # band +
#     layout = c(2, 3),
#     aspect = 0.45,
#     pars = pars, unit = "mm/y",
#     ylim = c(-65, 100), xlim = c(-180, 250),
#     sp.layout = sp_layout,
#     key.num2factor = TRUE,
#     colorkey = list(space = "right", height = 0.9),
#     brks = brks, colors = cols
#   ) +
#     theme_lattice(
#       key.margin = c(0, 1.5, 0, 0),
#       plot.margin = c(0, 4.5, 0.5, 1)
#     ) +
#     layer_title(y = 0.95) +
#     layer_statistic(x = 0.5, y = 0.1, cex = 1.4) +
#     layer_barchart(height = 0.2, width = 0.2) +
#     layer_latFreq(bbox =c(190, 247, -60, 90), unit = "native",
#                   is_spatial = TRUE,
#                   col.regions = c("red", "blue"),
#                   zlim = c(-1, 1),
#                   ylim = c(-60, 90))
#   outfile = glue::glue("Figure1_{bandName}_spatial trends of signals.pdf")
#   write_fig(p, outfile, 12, 7.5)
# }

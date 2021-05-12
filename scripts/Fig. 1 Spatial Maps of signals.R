# !/usr/bin/Rscript
# Dongdong Kong ----------------------------------------------------------------
source("scripts/main_pkgs.R")
library(foreach)

# ------------------------------------------------------------------------- 3. Figure
# load(file = 'Multimodel median E Trends from CIMP6 and GLEAM.RData')
load(file = "data-raw/Multimodel mean E Trends from CIMP6 and GLEAM.RData")
{
  Data <- Trends_ALL
  Data <- llply(1:5, function(j) {
    cbind(Data$E[, j], Data$Ev[, j], Data$Et[, j])
  })
  Data <- abind(Data, along = 2) %>% as.data.frame()
  attr(Data, "dimnames") <- NULL
  Names <- paste(paste0("(", letters[1:15], ")"), rep(names(Trends_ALL$E), each = 3), rep(c("E", "Et", "Ev"), 5))
  # Data <- data.frame(Data)
  names(Data) <- Names
  L <- 2
  Data[Data > L] <- L
  Data[Data < -L] <- -L
}

{
    library(sf)
    shp_cont <- read_sf("I:/Research/phenology/phenologyTP/data-raw/shp/continent.shp") %>%
        as_Spatial()
    sp_cont <- list("sp.lines", shp_cont, lwd = 0.2, first = F)
    # sp_sign = list("sp.polygons", poly_shade, first = FALSE, lwd = 0.1)
    # sp_sign,
    sp_layout = list(sp_cont)
}

# prepare plot data
{
  set_options(list(style = "EN"))
  grid <- expand.grid(s1 = seq(-179.5, 179.5, 1), s2 = seq(-89.5, 89.5, 1)) %>%
    cbind(I = 1:nrow(.), .) %>%
    df2sp(formula = ~ s1 + s2) %>%
    sf2::as_SpatialPixelsDataFrame()
  # grid2 <- melt(grid, id.vars = c('lon','lat'), variable.name = 'Factor', value.name = 'trends')
  df2 <- Trends_ALL %>%
    map(~ cbind(I = 1:nrow(.), .)) %>%
    melt_tree(c("band")) %>%
    melt(c("band", "I"), variable.name = "forcing") %>%
    data.table()
}

# Tables
{
  d_obs = df2[forcing == "Obs"] %>% dcast2(by = "band", value.var = "value") %>% data.table() %>%
    .[!is.na(E), ]
  d_cont = d_obs[, abs(.SD/E)*100, .SDcols = c("Et", "Ev")] %T>% summary()

  bands = c("E", "Et", "Ev") %>% set_names(., .)
  bandName = "Et"
  d_agree = foreach(bandName = bands) %do% {
    d_E = df2[band == bandName] %>% dcast2(by = "forcing", value.var = "value") %>% data.table() %>%
      .[!is.na(Obs) & Obs != 0, ]
    # tbl = d_E[, -(1:2)] %>% sign_agree()
    melt(d_E, c("band", "I", "Obs")) %>% data.table() %>%
      .[, sign_agree(Obs, value) %>% as.list, .(variable)]
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

for(bandName in bands[1:3]) {
  # load_all("I:/Research/phenology/latticeGrob.R")
  p <- sp_plot(grid,
    # df2,
    df2[band == bandName],
    layout = c(2, 3),
    formula = value ~ lon + lat | forcing, # band +
    aspect = 0.45,
    pars = pars, unit = "mm/y",
    ylim = c(-65, 100), xlim = c(-180, 250),
    sp.layout = sp_layout,
    key.num2factor = TRUE,
    colorkey = list(space = "right", height = 0.9),
    brks = brks, colors = cols
  ) +
    theme_lattice(
      key.margin = c(0, 1.5, 0, 0),
      plot.margin = c(0, 4.5, 0.5, 1)
    ) +
    layer_statistic(x = 0.5, y = 0.1, cex = 1.4) +
    layer_histFreq(height = 0.2, width = 0.2) +
    layer_latFreq(bbox =c(190, 247, -60, 90), unit = "native",
                  is_spatial = TRUE,
                  zlim = c(-1, 1),
                  ylim = c(-60, 90))
  outfile = glue::glue("Figure1_{bandName}_spatial trends of signals.pdf")
  write_fig(p, outfile, 12, 7.5)
}

## 2.2 visualization ------------------------------------------------------------
{
  pars <- list(title = list(x = -170, y = 100, cex = 1.4, adj = c(0, 1)))
  p <- sp_plot(grid,
               df2,
               # df2[band == "E" & forcing == "Obs"],
               # layout = c(2, 3),
               formula = value ~ lon + lat |  band + forcing,
               aspect = 0.45,
               pars = pars, unit = "mm/y",
               ylim = c(-65, 110), xlim = c(-180, 260),
               sp.layout = sp_layout,
               key.num2factor = TRUE,
               colorkey = list(space = "right", height = 0.9),
               brks = brks, colors = cols
  ) +
    theme_lattice(
      key.margin = c(0, 1.5, 0, 0),
      plot.margin = c(0, 5.5, 0.5, 1)
    ) +
    layer_statistic(x = 0.4, y = 0.1, cex = 1.4) +
    layer_histFreq(x = 0.06, y = 0.05, height = 0.2, width = 0.2, title = FALSE, xlabels = FALSE) +
    layer_latFreq(bbox =c(185, 247, -60, 90), unit = "native",
                  is_spatial = TRUE, ylabels = FALSE,
                  zlim = c(-1, 1),
                  ylim = c(-60, 90)) +
    layer_signPerc(x = 0.0, y = 0.55)
  outfile = glue::glue("Figure1_all_spatial trends of signals.pdf")
  write_fig(p, outfile, 14, 9.8)
}

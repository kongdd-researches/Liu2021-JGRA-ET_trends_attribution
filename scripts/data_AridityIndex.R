library(stringr)
library(foreach)
library(iterators)
library(matrixStats)
library(ncdf4)
library(terra)

str_year <- function(x) basename(x) %>% str_extract("\\d{4}")

## 1. MSWEP Prcp降水数据聚合到年，并重采样到1deg
files <- dir("F:/SciData/ET_products/Prcp/MSWEP_V280_monthly", full.names = TRUE)[-(1:11)]
lst_files <- split(files, str_year(files))
lst <- foreach(infiles = lst_files, i = icount()) %do% {
    runningId(i)
    arr = foreach(infile = infiles, j = icount()) %do% {
        raster(infile) %>% as.array()
    } %>% abind::abind(along = 3)
    arr_year = apply_3d(arr, FUN = rowSums2)
}

arr <- abind::abind(lst, along = 3) %>% aperm(c(2, 1, 3)) %>% flipud()
dates <- seq(as.Date("1980-01-01"), as.Date("2020-12-31"), by = "year")
ncwrite(list(Prcp = arr), "MSWEP_V280_yearly-(1980-2020).nc", "mm/year", dates = dates)

b <- brick("MSWEP_V280_yearly-(1980-2020).nc")
r_1deg <- aggregate(b, 5)
arr_1deg = r_1deg %>% raster2array()
ncwrite(list(Prcp = arr_1deg), "MSWEP_V280_yearly_0.5deg-(1980-2020).nc",
        "mm/year", dates = dates)

## 2. 修复GLEAM
## 3. 计算Aridity index
file = "F:/SciData/ET_products/已完成/GLEAM_3.5a_yearly/Ep_1980-2020_GLEAM_v3.5a_YR.nc"
# file_pet = fix_gleam(file)
# file_pet2 = aggregate_nc(file_pet, 4)
# {
#     check_raster("INPUTS/MSWEP_V280_yearly_1.0deg-(1980-2020).nc")
#     check_raster(file_pet)
#     check_raster(file_pet2)
# }
file_pet2 = "F:/SciData/ET_products/已完成/GLEAM_3.5a_yearly/Ep_1980-2020_GLEAM_v3.5a_YR_fixed_1deg.nc"
pet <- ncread(file_pet2)$data[[1]]
# image(pet[,,1] )

file_prcp = "INPUTS/MSWEP_V280_yearly_1.0deg-(1980-2020).nc"
prcp <- ncread(file_prcp)$data[[1]]
# image(prcp[,,1])

AI = prcp/pet
res = apply(AI, c(1, 2), slope_p)
res = plyr::aaply(AI, c(1, 2), slope_mk, .progress = "text")
ai = apply_3d(AI) %>% spdata_array
# ncread(file_prcp, "lat")
r_ai <- as_raster(ai)
writeRaster(r_ai, "INPUTS/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif")

# show aridity spatial distribution
library(rgdal)
library(lattice.layers)
library(rcolors)
library(sf)
sp_cont <- list("sp.lines", get_continent(), lwd = 0.5, first = F)
sp_layout = list(sp_cont)

brks = c(-Inf, 0.2, 0.5, 0.65, Inf)
levs = c("arid", "semi-arid", "sub-humid", "humid")

g <- readGDAL("INPUTS/AridityIndex_MSWEP-prcp_div_GLEAM-Ep_1980-2020.tif")
g$band1 %<>% clamp(c(0, 10), TRUE) %>%
    cut(brks)

# {
#     colorkey.param$at <- seq_len(length(colorkey.param$labels$labels) + 1) - 0.5
#     draw.colorkey(colorkey.param) %>% write_fig("a.pdf")
# }
# trace(sp:::spplot.grid, edit =T)

{
    # load_all("I:/Research/phenology/lattice.layers.R")
    # brks = c(-Inf, 0, 0.05, 0.2, 0.5, 0.65, Inf)
    # brks2 = c(0.05, 0.2, 0.5, 0.65, 1, 2, 5)
    # brks2 = c(0.05, 0.2, 0.65, 1)
    #
    # sub- humid (0.65 > AI ≥ 0.5), semi- arid (0.5 > AI ≥ 0.2),
    # arid (0.2 > AI ≥ 0.05) and hyper- arid (AI < 0.05) regions
    nbrk = length(brks) - 1
    n_more = 2
    cols <- get_color(rcolors$amwg256, nbrk+n_more) %>% .[-(1:n_more)] %>% rev()
    p <- sp_plot(g,
                 # brks = brks,
                 colors = cols,
                 xlim = c(-180, 240),
                 ylim = c(-60, 90),
                 aspect = 0.5,
                 sp.layout = sp_layout,
                 key.num2factor = TRUE) +
        layer_title(x = 0, y = 1, labels = c("AI = P/PET")) +
        layer_latFreq(bbox = c(185, 240, -60, 90), zlim = c(-2, 2),
                      unit = "native", is_spatial =TRUE) +
        layer_barchart(x = 0.01, y = 0.05, width = 0.22) +
        layer_contourf(brks = brks2)
        # layer_statistic(x = 0.4, y = 0.1, cex = 1.4)
    write_fig(p, "Figure_S01_AridityIndex_SpatialDistribution2.pdf")
}

# {
#     # r <- make_grid(range = c(-180, 180, -90, 90), 0.1)
#     r@data <- as.numeric(arr_year %>% t() %>% flipud()) %>% data.table(x = .)
#     r2 <- raster(r)
#     plot(r2)
# }
#
# r <- raster(infile) #%>% as.array()
# values(r) <- as.numeric(arr_year %>% t())
# plot(r)
# write_fig(r)

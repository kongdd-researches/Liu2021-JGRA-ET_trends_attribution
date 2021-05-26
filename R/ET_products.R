trans_array <- function(x) x %>% aperm(c(2, 1, 3))

raster2array <- function(r) {
    as.array(r) %>%
        trans_array() %>%
        flipud()
}

check_raster <- function(file) {
    par.old = par(mfrow= c(2, 1))
    on.exit(par(par.old))
    raster(file) %>% raster::plot()

    arr <- ncread(file)$data[[1]]
    image(arr[, , 1])
}

array2raster <- function(arr) {
    arr %<>% spdata_array()
    lapply(1:dim(arr)[3], function(i) {
        raster(vals = ai[,,1] %>% t() %>% fliplr())
    }) %>% do.call(stack, .)
}

#' @import ncdf4
#' @importFrom nctools ncread nc_date ncdim_def_lonlat
fix_gleam <- function(file) {
    file_new = file %>% gsub(".nc$", "_fixed.nc", .)
    if (file.exists(file)) file.remove(file_new)

    info <- get_ncdim(file)
    arr <- ncread(file, 1)$data[[1]] %>% raster2array() # looks normal, image(arr[,,1])
    # ncwrite(list(arr) %>% set_names(names(fid$var)), file_new, unit, dates = dates) # kg m-2 = mm
    ncwrite(list(arr) %>% set_names(info$varnames),
        file_new, info$unit, dims = info$dims, attrs = info$attrs)
    file_new
}

get_ncdim <- function(file, sort.lat = TRUE) {
    fid = nc_open(file)
    on.exit(nc_close(fid))

    dates <- nc_date(file) %>%
        as.Date.POSIXct() %>%
        {
            make_date(year(.), month(.), 1)
        }
    lon <- ncread(file, "lon")
    # make sure increase order, this why gleam need to fix
    lat <- ncread(file, "lat")
    if (sort.lat) lat %<>% sort() # increasing order
    dims <- ncdim_def_lonlat(lon, lat, dates)

    unit = purrr::map_chr(fid$var, "units")
    attrs = ncatt_get(fid, 0) %>% unlist()

    listk(varnames = names(fid$var), unit,
          lon, lat, dates = dates,
          attrs, dims)
}

#' @importFrom raster aggregate writeRaster as.array
aggregate_nc <- function(file, fact = 5, fun=mean, ...) {
    cellsize = ncread(file, "lon") %>% diff() %>% median()
    dates <- nc_date(file)
    cellsize2 = cellsize*fact

    file_new = gsub(".nc$", paste0("_", cellsize2, "deg.nc"), file)
    b <- brick(file)
    b2 <- aggregate(b, fact, fun = fun, ...)
    arr <- b2 %>% raster2array()

    info <- get_ncdim(file)
    ncwrite(list(arr) %>% set_names(info$varnames),
        file_new, info$unit, dates = info$dates, attrs = info$attrs) # kg m-2 = mm
    file_new
}

# arr <- values(b2) %>% set_dim(dim(b2))
# range = b2@extent %>% {c(.@xmin, .@xmax, .@ymin, .@ymax)}
# writeRaster(b2, outfile, overwrite = TRUE)
# arr2 <- brick(outfile) %>% as.array()

# library(raster)
# 
# rr_scd <- raster("data-raw/2000-10-01_2019-09-30.tif")
# 
# writeRaster(rr_scd,
#             filename = "data/modis_scd.tif",
#             overwrite = T)

library(stars)

rs_scd <- read_stars("data-raw/2000-10-01_2019-09-30.tif")

rs_scd_int <- round(rs_scd)
# rs_scd_round <- round(rs_scd, 1)

write_stars(rs_scd_int,
            dsn = "data/modis_scd_250m_laea.tif",
            overwrite = T,
            # type = "Int16",
            options = c("COMPRESS=LZW"))


rs_scd_anom <- read_stars("data-raw/2000-10-01_2019-09-30_bin100m.tif")
rs_scd_anom_int <- round(rs_scd_anom)


write_stars(rs_scd_anom_int,
            dsn = "data/modis_scd_anom_250m_laea.tif",
            overwrite = T,
            options = c("COMPRESS=LZW"))



# agg and project for leaflet ---------------------------------------------


# (EPSG:3857)
# 
# dxy_old <- st_dimensions(rs_scd)$x$delta
# 
# dest <- st_as_stars(st_bbox(rs_scd), dx = dxy_old*4, dy = dxy_old*4)
# 
# rs_scd_1km <- st_warp(rs_scd, dest_leaf,
#                       use_gdal = T, method = "average")
# rs_scd_1km_leaf <- st_warp(rs_scd_1km, dest_leaf, use_gdal = T)
# 
# rs_scd_1km <- round(rs_scd_1km)
# 
# 
# write_stars(rs_scd_1km,
#             dsn = "data/modis_scd_anom_1km.tif",
#             overwrite = T,
#             options = c("COMPRESS=LZW"))




library(raster)

rr_scd <- raster("data-raw/2000-10-01_2019-09-30.tif")
rr_scd_1km <- aggregate(rr_scd, 4)
# rr_scd_1km_leaf <- projectRaster(rr_scd_1km, crs = st_crs(3857)$proj4string)
rr_scd_1km_leaf2 <- leaflet::projectRasterForLeaflet(rr_scd_1km, "bilinear")

writeRaster(round(rr_scd_1km_leaf2),
            filename = "data/modis_scd_1km_webmerc.tif",
            overwrite = T)

rr_scd_anom <- raster("data-raw/2000-10-01_2019-09-30_bin100m.tif")
rr_scd_anom_1km <- aggregate(rr_scd_anom, 4)
rr_scd_anom_1km_leaf2 <- projectRasterForLeaflet(rr_scd_anom_1km, "bilinear")


writeRaster(round(rr_scd_anom_1km_leaf2),
            filename = "data/modis_scd_anom_1km_webmerc.tif",
            overwrite = T)

# get elevation background -----------------------------------------------------------


library(raster)

# rr_elev <- raster("data-raw/eurac_modis_altitude_laea.tif")
rr_elev <- raster("data-raw/srtm30_alps.tif")
rr_elev_4km <- aggregate(rr_elev, 5)
rr_elev_8km <- aggregate(rr_elev, 10)


mat_4km <- as.matrix(rr_elev_4km)
rowcol_3000 <- which(mat_4km > 3000, arr.ind = T)
rowcol_3000[, 2] %>% table

# mycol <- 100*5
mycol <- 107*5
# mycol <- 250

ns_elev1 <- getValuesBlock(rr_elev, 
                           row = 1, nrows = nrow(rr_elev),
                           col = mycol, ncols = 1)
plot(ns_elev1, type = "l")
plot(zoo::rollmean(ns_elev1, 15), type = "l")


ns_elev2 <- getValuesBlock(rr_elev_4km, 
                           row = 1, nrows = nrow(rr_elev_4km),
                           col = mycol/5, ncols = 1)
plot(ns_elev2, type = "l")

ns_elev3 <- getValuesBlock(rr_elev_8km, 
                           row = 1, nrows = nrow(rr_elev_8km),
                           col = mycol/10, ncols = 1)
plot(ns_elev3, type = "l")


tbl_elev <- tibble::tibble(ii = 1:length(ns_elev1),
                           elev_raw = ns_elev1,
                           elev_rollmean = zoo::rollmean(ns_elev1, 15, na.pad = T))

saveRDS(tbl_elev, "data/mountain-background.rds")

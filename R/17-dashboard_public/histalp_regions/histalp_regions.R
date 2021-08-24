# digitalize snow regions histalp zamg

# libs
library(sf)
library(mapedit)
library(mapview)
library(foreach)
library(dplyr)
library(data.table)


# zamg histalp publication/visualization
# http://www.zamg.ac.at/histalp/project/maps/gar_reg.php

# get extent of station network in clirsnow
# taken from here: https://gitlab.inf.unibz.it/REMSEN/alpine_wide_snow_trends/-/blob/master/R/15-dashboard/dash-06-final.Rmd
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")
dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
mat_clust3_eof <- sinkr_eof$u

dat_regions <- foreach(
  kk = 2:5,
  .final = rbindlist
) %do% {
  
  km_fit <- kmeans(mat_clust3_eof[, 1:kk], kk)
  stn_clust <- km_fit$cluster
  data.table(kk = kk, 
             i_cluster = stn_clust,
             Name = colnames(mat_eof))
  
}

dat_plot_regions <- dat_regions %>% merge(dat_meta_clust, by = "Name")
dat_plot_regions[, Region := factor(i_cluster)]
dat_plot_regions[, label := paste0(
  Name, ", ", Elevation, "m, ", round(Longitude, 3), "°?E ", round(Latitude, 3), "°?N"
)]

sf_regions <- st_as_sf(dat_plot_regions,
                       coords = c("Longitude", "Latitude"),
                       crs = 4326)

ext_stations = st_as_sfc(st_bbox(sf_regions))

# get modis extent
pth_scd = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif"
scd = stars::read_stars(pth_scd)
ext_scd = st_as_sfc(st_bbox(scd))
mapview(ext_scd) + mapview(ext_stations)

ext_combined = st_intersection(ext_scd, st_transform(ext_stations, crs = st_crs(ext_scd)))
mapview(ext_combined)

# download official regions
# wget http://www.zamg.ac.at/histalp/download/crsm/Shape_CRSM.ZIP
shp = read_sf("R/17-dashboard_public/histalp_regions/Shape_CRSM.shp")
mapview(shp) + mapview(ext_combined)

# temperature and precipitation were edited in qgis according to the image
# https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.1377
# http://www.zamg.ac.at/histalp/project/maps/gar_reg.php

precip = read_sf("R/17-dashboard_public/histalp_regions/Shape_CRSM_PR.shp")
temp = read_sf("R/17-dashboard_public/histalp_regions/Shape_CRSM_T.shp")
mapview(shp) + mapview(precip) + mapview(temp) + mapview(ext_combined)

# # try to get rid of slivers... not working so far
# temp_clean = temp %>% st_transform(crs = st_crs(3035)) %>% 
#   st_snap(x = ., y = ., tolerance = 1) %>% 
#   st_union()
# mapview(temp_clean)
# 
# temp_clean = temp %>% st_transform(crs = st_crs(3035)) %>%  st_buffer(2000)
# temp_clean = temp_clean %>% 
#   st_combine() %>% 
#   st_union()
# temp_clean = st_difference(temp_clean, temp_clean) %>%
#   st_combine() %>%
#   st_union()

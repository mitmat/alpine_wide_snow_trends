# SCD Leaflet

# libs
library(stars)
library(sf)
library(mapview)
library(dplyr)

# load scd
pth_scd = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif"
scd = stars::read_stars(pth_scd)

# recalc the values
scd = round(scd, 0)
scd


# map it
mapview(scd)

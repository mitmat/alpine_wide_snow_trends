# 3d vis of alps with snowcover

## libs
library(raster)
library(quadmesh)
library(rgl)
library(colourvalues)
library(sf)
library(mapview)

# examples --------------------------------------------------------------------- 
# from here: https://rpubs.com/cyclemumner/geomesh-r
## volcano
rvolcano <- raster(volcano)
qm_volcano <- quadmesh(rvolcano)
rgl.clear()
shade3d(qm_volcano, col = "grey"); aspect3d(1, 1, 0.25)
rglwidget()


## etopo is a test copy of a partial world DEM
data("etopo", package = "quadmesh")
qm_etopo <- quadmesh(crop(etopo, extent(80, 160, -50, 10)))
qm_etopo$material$col <- colourvalues::colour_values(qm_etopo$vb[3, qm_etopo$ib])
rgl.clear()
shade3d(qm_etopo); aspect3d(1, 1, .2)
rglwidget()


# modis snow -------------------------------------------------------------------
# snow cover duration mean 20yrs
pth_scd = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif"
scd = raster::stack(pth_scd)
# modis dem
pth_dem = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/00_aux/eurac_modis_altitude_laea.tif"
dem = raster::stack(pth_dem)
dem[dem == -32768] = NA #  or is 0 betta??? for 3d model

# subset
sub = mapedit::drawFeatures()
sub = c(xmin = 11.67, ymin = 46.49, xmax = 11.77, ymax = 46.57) # plattkofel
sub = c(xmin = 10.20, ymin = 45.61, xmax = 12.71, ymax = 47.00) # st
sub = sf::st_bbox(sub, crs = sf::st_crs(4326))
mapview::mapview(sub)
sub = sf::st_transform(st_as_sfc(sub), crs = st_crs(dem))

dem_sub = crop(dem, as(sub, "Spatial"))
scd_sub = crop(scd, as(sub, "Spatial"))
plot(dem_sub)
plot(scd_sub)

# quadmesh
qm_dem_sub <- quadmesh(dem_sub)
rgl.clear()
shade3d(qm_dem_sub, col = "grey"); aspect3d(1, 1, 0.075) # last value is z scalierung, kleiner ist abflachender
rglwidget()


# quadmesh
qm_dem_sub <- quadmesh(dem_sub)
qm_scd_sub <- quadmesh(scd_sub)
qm_dem_sub$material$col <- colourvalues::colour_values(qm_scd_sub$vb[3, qm_scd_sub$ib])
rgl.clear()
shade3d(qm_dem_sub); aspect3d(1, 1, 0.25)
rglwidget()



# mapview ----
mapview(scd_sub, layer.name = "X2000.10.01_2019.09.30")





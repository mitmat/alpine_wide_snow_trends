# mapdeck 3d

# libraries --------------------------------------------------------------------
# install.packages("mapdeck")
library("mapdeck")

library(raster)
library(quadmesh)
library(rgl)
library(colourvalues)
library(sf)

# token ------------------------------------------------------------------------
key <-  "pk.eyJ1IjoicHplbGxuZXIiLCJhIjoiY2twaTE3bGdqMGlsYjJvbzhqY3Fod282biJ9.llUDfOVrtkTxvXg48S3hSQ"   ## put your own token here
set_token(key)
mapdeck_tokens()

# basemap mapdeck 
# mapdeck()

# eurac data -------------------------------------------------------------------

# snow cover duration mean 20yrs
pth_scd = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif"
scd = raster::stack(pth_scd)

# modis dem
pth_dem = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/00_aux/eurac_modis_altitude_laea.tif"
dem = raster::stack(pth_dem)
dem[dem == -32768] = NA #  or is 0 betta??? for 3d model

# subset
#sub = mapedit::drawFeatures()
sub = c(xmin = 11.67, ymin = 46.49, xmax = 11.77, ymax = 46.57) # plattkofel
#sub = c(xmin = 10.20, ymin = 45.61, xmax = 12.71, ymax = 47.00) # st
sub = sf::st_bbox(sub, crs = sf::st_crs(4326))
#mapview::mapview(sub)
sub = sf::st_transform(st_as_sfc(sub), crs = st_crs(dem))

dem_sub = crop(dem, as(sub, "Spatial"))
scd_sub = crop(scd, as(sub, "Spatial"))
plot(dem_sub)
plot(scd_sub)

# turn into df
dem_sub
dem_sub_df = as.data.frame(raster::projectRaster(from = dem_sub, crs = 4326), 
                           xy = TRUE
                           , centroids = TRUE
                           ) 
                           
summary(dem_sub_df$eurac_modis_altitude_laea)


# add_mesh ---------------------------------------------------------------------
## built in example
m <- melbourne_mesh
m$vb[3, ] <- m$vb[3, ] * 50
str(m)

# turn into mesh
m_dem <- quadmesh(dem_sub) # doesnt seem that quadmesh is supported, only mesh3d

wire3d(m_dem)
tm <- triangmesh(dem_sub)


str(m_dem)
# TODO: ALIGN THE STRUCTURES
str(tm)
str(m)

## visualize
mapdeck() %>%
  add_mesh(
    data = m
  )


# add_grid ---------------------------------------------------------------------
# eurac example
mapdeck( style = mapdeck_style("dark"), pitch = 45) %>%
  add_grid(
    data = dem_sub_df
    , lat = "y"
    , lon = "x"
    , layer_id = "hex_layer"
    , cell_size = 250 
    , elevation_scale = 5000
    , legend = T
    , elevation_function = "mean"
    , elevation = "eurac_modis_altitude_laea"
  )




# csv
df <- read.csv(paste0(
  'https://raw.githubusercontent.com/uber-common/deck.gl-data/master/',
  'examples/3d-heatmap/heatmap-data.csv'
))

df <- df[ !is.na(df$lng ), ]
head(df)

mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = df
    , lat = "lat"
    , lon = "lng"
    , cell_size = 5000
    , elevation_scale = 50
    , layer_id = "grid_layer"
    , auto_highlight = TRUE
  )

# color and elevation 
## using colour and elevation functions, and legends
df$val <- sample(1:10, size = nrow(df), replace = T)
head(df)


mapdeck( style = mapdeck_style("dark"), pitch = 45) %>%
  add_grid(
    data = df
    , lat = "lat"
    , lon = "lng"
    , layer_id = "hex_layer"
    , elevation_scale = 100
    , legend = T
    , colour_function = "max"
    , colour = "val"
  )

mapdeck( style = mapdeck_style("dark"), pitch = 45) %>%
  add_grid(
    data = df
    , lat = "lat"
    , lon = "lng"
    , layer_id = "hex_layer"
    , elevation_scale = 10
    , legend = T
    , elevation_function = "mean"
    , elevation = "val"
  )

# using sf object
library(sfheaders)
sf <- sfheaders::sf_point( df, x = "lng", y = "lat")


mapdeck( style = mapdeck_style("dark"), pitch = 45 ) %>%
  add_grid(
    data = sf
    , cell_size = 5000
    , elevation_scale = 50
    , layer_id = "grid_layer"
    , auto_highlight = TRUE
  )



# add_terrain ------------------------------------------------------------------
elevation <- 'https://raw.githubusercontent.com/visgl/deck.gl-data/master/website/terrain.png'
texture <- 'https://raw.githubusercontent.com/visgl/deck.gl-data/master/website/terrain-mask.png'
bounds <- c(-122.5233, 37.6493, -122.3566, 37.8159)

mapdeck() %>%
  add_terrain(
    , elevation_data = elevation
    , elevation_decoder = c(1,0,0,0)
    , texture = texture
    , bounds = bounds
    , max_error = 1
  )






library(sf)
library(dplyr)
library(mapview)


# test plots --------------------------------------------------------------


load("data/viz_past.Rdata")

mv5_sf_cur
histalp_reg_p



ggplot()+
  borders()+
  geom_sf(data = mv5_sf_cur, aes(colour = cluster_fct))+
  scale_color_brewer(palette = "Set1")+
  theme_minimal()

ggplot()+
  geom_sf(data = histalp_reg_p[c(2,3), ])

ggplot()+
  geom_sf(data = histalp_reg_p)

ggplot()+
  geom_sf(data = histalp_reg_p)+
  facet_wrap(~region)

ggplot()+
  geom_sf(data = histalp_reg_t[c(2,3), ])

ggplot()+
  geom_sf(data = histalp_reg_t)

ggplot()+
  geom_sf(data = histalp_reg_t)+
  facet_wrap(~region)


ggplot()+
  geom_sf(data = histalp_reg_t[c(2,3), ], 
          aes(colour = "Temperature"),
          fill = NA)+
  geom_sf(data = histalp_reg_p[c(2,3), ], 
          aes(colour = "Precipitation"),
          fill = NA)


# cluster simple -----------------------------------------------------------------

dat_meta_cluster <- readRDS("data/meta-with-cluster-01.rds")
sf_meta <- st_as_sf(dat_meta_cluster,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)


ggplot()+
  borders()+
  geom_sf(data = sf_meta, aes(colour = cluster_fct, shape = cluster_fct))+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_sf(xlim = range(dat_meta_cluster$Longitude), ylim = range(dat_meta_cluster$Latitude))+
  theme_bw()+
  theme(# legend.position = c(0.65, 0.15),
    legend.position = "top",
    # legend.title = element_blank(),
    legend.background = element_rect(colour = "grey"),
    legend.direction = "horizontal",
    legend.box.just = "right")+
  scale_shape("Snow depth clusters")+
  scale_color_brewer("Snow depth clusters", palette = "Set1")+
  guides(color = guide_legend(override.aes = list(size = 4)))


sf_borders2 <- sf_borders %>% 
  dplyr::mutate(geom = st_cast(geom, "MULTILINESTRING")) %>% 
  # st_crop(sf_meta)
  st_crop(c(xmin = 4.5, ymin = 42.9, xmax = 18, ymax = 49.5))
  

ggplot()+
  # borders()+
  geom_sf(data = sf_borders2, fill = NA)+
  geom_sf(data = sf_meta, aes(colour = Region, shape = Region))+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  # coord_sf(xlim = range(dat_meta_cluster$Longitude), ylim = range(dat_meta_cluster$Latitude))+
  theme_bw()+
  # theme(# legend.position = c(0.65, 0.15),
  #   legend.position = "top",
  #   # legend.title = element_blank(),
  #   legend.background = element_rect(colour = "grey"),
  #   # legend.direction = "horizontal",
  #   legend.box.just = "right")+
  scale_shape("")+
  scale_color_brewer("", palette = "Set1")+
  ggtitle("Regionalization of snow depth stations")

# mapview histalp ---------------------------------------------------------

sf_histalp <- readRDS("data/sf_histalp_extended.rds")

ggplot()+
  geom_sf(data = sf_histalp[c(1,3), ])

mapview(sf_histalp[c(1,3), ])+
  mapview(histalp_reg_t[c(2,3), ])+
  mapview(histalp_reg_p[c(2,3), ])


sf_lines_crs <- sf_histalp[c(1,3), ] %>% 
  st_cast("MULTILINESTRING") %>%
  summarise(geometry = st_combine(geometry)) %>% 
  st_crop(c(xmin = 3, ymin = 42.2, xmax = 20, ymax = 49)) 


sf_lines_t <- histalp_reg_t[c(2,3), ]%>%
  st_cast("MULTILINESTRING") %>%
  summarise(geometry = st_combine(geometry)) %>% 
  st_crop(c(xmin = 4.1, ymin = 43.1, xmax = 19, ymax = 48.7))


sf_lines_p <- histalp_reg_p[c(2,3), ]%>%
  st_cast("MULTILINESTRING") %>%
  summarise(geometry = st_combine(geometry)) %>% 
  st_crop(c(xmin = 4.1, ymin = 43.1, xmax = 19, ymax = 48.7)) 


mapview(sf_lines_crs, 
        layer.name = "HISTALP regions (overall)",
        color = "black",
        label = "summary climatic boundaries")+
  mapview(sf_lines_t, 
          layer.name = "HISTALP regions temperature", 
          color = "#b30000",
          label = "temperature boundaries")+
  mapview(sf_lines_p, 
          layer.name = "HISTALP regions Precipitation", 
          color = "#045a8d",
          label = "precipitation boundaries")



# eo scd clim -----------------------------------------------------------------

rs_clim <- read_stars(here::here("data/modis_scd.tif"))
rs_clim_anom <- read_stars(here::here("data/modis_scd_anom.tif"))

ds_stars <- 10 # for template
# ds_stars <- 5 # for production 2-5



##graticules 

prj <- st_crs(rs_clim)$proj4string

lons <- seq(10, 12, by = 2)
lats <- seq(46, 48, by = 1)
xl <- range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
grat <- graticule::graticule(lons, lats, proj = prj, xlim = xl, ylim = yl)
# labs <- graticule_labels(lons, lats, xline = min(xl), yline = max(yl), proj = prj)




# plots

ggplot()+
  geom_sf(data = st_as_sf(grat), colour = NA)+
  geom_stars(data = rs_clim, downsample = ds_stars)+
  scico::scale_fill_scico("[days]", limits = c(0, 366),
                   palette = "davos", na.value = NA, direction = 1)+
  coord_sf(expand = F)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  ggtitle("Average Snow Cover Duration 2000-2020")


ggplot()+
  geom_sf(data = st_as_sf(grat), colour = NA)+
  geom_stars(data = rs_clim_anom, downsample = ds_stars)+
  scico::scale_fill_scico("[days]", 
                          palette = "vik", na.value = NA, direction = -1,
                          rescaler = scales::rescale_mid)+
  coord_sf(expand = F)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  ggtitle("Anomaly by Elevation [days]",
          "Pixel SCD (snow cover duration) minus the areal average SCD of similarelevation (+-50m)")



# eo scd clim as leaflet --------------------------------------------------


rr_scd_1km <- raster(here::here("data/modis_scd_1km_webmerc.tif"))
rr_scd_anom_1km <- raster(here::here("data/modis_scd_anom_1km_webmerc.tif"))
# rs_clim_anom <- read_stars(here::here("data/modis_scd_anom.tif"))


leaflet(rr_clim) %>% 
  addRasterImage()


leaflet() %>% 
  addProviderTiles("Esri.NatGeoWorldMap") %>% 
  addRasterImage(rr_scd_1km_leaf2, 
                 project = F)

mapview(rr_scd_1km_leaf2)
mapview(rr_scd_1km)



# pal_scd <- colorNumeric("Blues", 0:366, na.color = NA)
pal_scd <- colorNumeric(scico::scico(palette = "davos", 10),
                        c(0, 366), 
                        na.color = NA)


max_diff <- max(abs(rr_scd_anom_1km[]), na.rm = T)

pal_scd_anom <- colorNumeric(scico::scico(palette = "vik", 11),
                             c(-max_diff, max_diff), 
                             na.color = NA,
                             reverse = T)

lf_scd <-
leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addRasterImage(rr_scd_1km, 
                 opacity = 0.8,
                 colors = pal_scd,
                 project = F) %>% 
  addLegend(pal = pal_scd, 
            values = values(rr_scd_1km),
            opacity = 0.8,
            title = "SCD [days]")


lf_scd_anom <-
leaflet() %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addRasterImage(rr_scd_anom_1km, 
                 opacity = 0.8,
                 colors = pal_scd_anom,
                 project = F) %>% 
  addLegend(pal = pal_scd_anom, 
            values = values(rr_scd_anom_1km),
            opacity = 0.8,
            title = "dSCD [days]")


sync(lf_scd, lf_scd_anom)



# explore boundaries ------------------------------------------------------


leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topomap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>% 
  
  addRasterImage(rr_scd_1km,
                 opacity = 0.8,
                 colors = pal_scd,
                 project = F,
                 group = "SCD") %>%
  addLegend(pal = pal_scd,
            values = values(rr_scd_1km),
            opacity = 0.8,
            title = "SCD [days]",
            position = "topleft") %>%

  addRasterImage(rr_scd_anom_1km,
                 opacity = 0.8,
                 colors = pal_scd_anom,
                 project = F,
                 group = "SCD anomalies") %>%
  addLegend(pal = pal_scd_anom,
            values = values(rr_scd_anom_1km),
            opacity = 0.8,
            title = "SCD anomalies [days]",
            position = "topleft") %>%

  addCircleMarkers(data = sf_meta,
                   stroke = F,
                   fillOpacity = 0.8,
                   radius = 5,
                   color = ~ mv5_leaf_col(Region),
                   group = "In-situ snow depth regions") %>% 
  addLegend(pal = mv5_leaf_col, 
            values = sf_meta$Region,
            opacity = 0.8,
            title = "In-situ snow depth regions",
            position = "bottomleft") %>% 


  addPolylines(data = sf_lines_crs,
               color = "black",
               opacity = 0.8,
               group = "HISTALP Summary") %>% 
  addPolylines(data = sf_lines_t,
               color = "#b30000",
               opacity = 0.8,
               group = "HISTALP Temperature") %>% 
  addPolylines(data = sf_lines_p,
               color = "#045a8d",
               opacity = 0.8,
               group = "HISTALP Precipitation") %>% 
  addLegend(colors = c("black",
                       "#b30000",
                       "#045a8d"),
            labels = c("summary climatic boundaries",
                       "temperature boundaries",
                       "precipitation boundaries"),
            opacity = 0.8,
            title = "HISTALP",
            position = "bottomleft") %>% 
  
  addLayersControl(baseGroups = c("CartoDB", "Topomap", "WorldImagery"),
                   overlayGroups = c("SCD", 
                                     "SCD anomalies",
                                     "In-situ snow depth regions",
                                     "HISTALP Summary", 
                                     "HISTALP Temperature", 
                                     "HISTALP Precipitation"),
                   position = "topright",
                   options = layersControlOptions(collapsed = FALSE)) %>% 
  hideGroup("SCD") %>%
  hideGroup("SCD anomalies")







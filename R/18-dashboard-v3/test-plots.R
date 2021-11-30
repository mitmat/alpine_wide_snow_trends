
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


# highcharter clim stn try ----------------------------------------------------

# geojs_borders2 <- geojsonsf::sf_geojson(sf_borders2)

st_write(sf_borders2, "data/borders.geojson")
# geojs_borders <- geojsonio::geojson_read("data/borders.geojson")
geojs1 <- jsonlite::fromJSON("data/borders.geojson")

geojs_borders <- jsonlite::fromJSON("data/borders.geojson", simplifyVector = FALSE) %>% 
  geojsonio::as.json()

highchart(type = "map") %>% 
  hc_chart(backgroundColor = "#161C20") %>% 
  # hc_add_series(mapData = geojs_borders)
  hc_add_series(mapData = geojs_borders,
                type = "mapline",
                lineColor = "#252525",
                lineWidth = 2)

highchart(type = "map") %>% 
  # hc_add_series(mapData = geojs_borders) %>% 
  hc_add_series(data = dat_meta_clust[1:100, ],
                type = "mappoint")

hcmap() %>% 
  # hc_add_series(mapData = geojs_borders) %>% 
  hc_add_series(data = dat_meta_clust[1:100, .(lat = Latitude, lon = Longitude, Name, cluster_fct)],
                type = "mappoint") %>% 
  hc_mapNavigation(enabled = TRUE)




# highcharter clim stn ok ----------------------------------------------------

cols_cluster <- setNames(scales::brewer_pal(palette = "Set1")(5),
                         c("NW", "NE", "North & high Alpine", "South & high Alpine", "SE"))

st_write(sf_borders2, "data/borders.geojson")

sf_meta <- sf_meta %>% mutate(color = cols_cluster[cluster_fct])
st_write(sf_meta, "data/meta.geojson")

geojs_borders <- jsonlite::fromJSON("data/borders.geojson", simplifyVector = FALSE) %>% 
  geojsonio::as.json()

geojs_meta <- jsonlite::fromJSON("data/meta.geojson", simplifyVector = FALSE) %>% 
  geojsonio::as.json()

highchart(type = "map") %>% 
  # hc_chart(backgroundColor = "#161C20") %>% 
  hc_add_series(mapData = geojs_borders,
                type = "mapline",
                lineColor = "#252525",
                lineWidth = 2) %>%
  hc_add_series(data = geojs_meta,
                type = "mappoint",
                # hcaes = hcaes(color = cluster_fct),
                # colorKey = "cluster_fct",
                tooltip = list(pointFormat = "{point.properties.color}")) %>% 
  hc_mapNavigation(enabled = TRUE)



highchart(type = "map") %>% 
  # hc_chart(backgroundColor = "#161C20") %>% 
  hc_add_series(mapData = geojs_borders,
                type = "mapline",
                lineColor = "#252525",
                lineWidth = 2) %>%
  hc_add_series(data = as_tibble(sf_meta),
                type = "point",
                # hcaes = hcaes(color = cluster_fct),
                # colorKey = "cluster_fct",
                tooltip = list(pointFormat = "{point.properties.color}")) %>% 
  hc_mapNavigation(enabled = TRUE)

highchart() %>% 
  hc_add_series(dat_meta_cluster,
                "point",
                hcaes(Longitude, Latitude))


# echarts stn -------------------------------------------------------------

library(echarts4r)



dat_meta_cluster %>% 
  group_by(cluster_fct) %>% 
  e_chart(Longitude) %>% 
  e_geo(roam = T,
        boundingCoords = list(c(4.5, 49.7), c(18, 43))) %>%
  e_scatter(Latitude,
            coord_system = "geo") 
  
dat_meta_cluster %>% 
  group_by(cluster_fct) %>% 
  e_chart(Longitude) %>% 
  e_geo(roam = T,
        # tooltip = list(show = F),
        boundingCoords = list(c(4.5, 49.7), c(18, 43))) %>%
  e_scatter(Latitude,
            coord_system = "geo") %>% 
  e_tooltip()


# mountain elev background (highcharter) ------------------------------------------------

tbl_djf_rel <- tribble(
  ~x, ~y, ~ns, ~value, ~elev_range, 
  200, 500,  "North", -7.2, "0-1000m",
  420, 500,  "South", -8.7, "0-1000m",
  220, 1500,  "North", -3.6, "1000-2000m",
  380, 1500,  "South", -6.5, "1000-2000m",
  240, 2500,  "North", -2.5, "2000-3000m",
  360, 2500,  "South", 0.2, "2000-3000m",
  250, 3500,  "North", 0, ">3000m",
  320, 3500,  "South", 0, ">3000m"
) %>% 
  mutate(label = if_else(y < 3000, sprintf("%+.1f%%", value*49/10), "?"),
         value_size = if_else(y < 3000, abs(value), 0.001))




library(highcharter)

tbl_elev <- readRDS("data/mountain-background.rds")
tbl_elev_sub <- tbl_elev[170: 450, ] %>% 
  dplyr::mutate(elev_scaled = ifelse(elev_rollmean > 1000,
                                     elev_rollmean/max(elev_rollmean)*4500,
                                     elev_rollmean))



hchart(tbl_elev_sub, 
       "line",
       hcaes(ii, elev_scaled))

# line, north, south
# cols <- c("#969696", "#386cb0", "#fdc086")
# cols <- c("#969696", "#a6cee3", "#b2df8a")
cols <- c("#969696", "#1f78b4", "#33a02c")

highchart() %>% 
  # hc_add_theme(hc_theme_bloom()) %>% 
  hc_add_theme(hc_theme_hcrt()) %>% 
  hc_add_series(tbl_elev_sub,
                "line",
                hcaes(ii, elev_scaled),
                name = "Mountain",
                enableMouseTracking = F,
                showInLegend = F,
                states = list(inactive = list(opacity = 1))) %>% 
  hc_add_series(tbl_djf_rel,
                "scatter",
                hcaes(x, y, group = ns, size = value_size),
                dataLabels = list(enabled = T,
                                  align = "left",
                                  x = 20,
                                  # color = "grey",
                                  format = "{point.label}")) %>% 
  hc_colors(cols) %>%
  hc_legend(align = "center",
            verticalAlign = "top",
            x = -30) %>%
  hc_yAxis(title = list(text = "Elevation",
                        rotation = 0,
                        align = "high",
                        offset = 10,
                        y = 10),
           # breaks = c(0:3*1000),
           labels = list(format = "{value:f} m"),
           min = 0, max = 4700,
           # tickAmount = 2,
           tickInterval = 1000,
           endOnTick = F) %>% 
  hc_xAxis(visible = F) %>% 
  hc_title(text = "Change in average winter snow depth 1971-2019") %>% 
  hc_subtitle(text = "Winter is December - February, changes averaged for 1000 m elevation bands") %>% 
  hc_caption(text = strwrap(width = 1e6, 
                            "Changes are determined from linear regressions over the whole period, 
                            and averaged over all stations in 1000 m elevation bands. 
                            Over 3000 m, no stations with such long records exist.")) 







# test label

tbl_djf_rel <- tribble(
  ~x, ~y, ~ns, ~value, ~elev_range, 
  200, 500,  "North", -7.2, "0-1000m",
  420, 500,  "South", -8.7, "0-1000m",
  220, 1500,  "North", -3.6, "1000-2000m",
  380, 1500,  "South", -6.5, "1000-2000m",
  240, 2500,  "North", -2.5, "2000-3000m",
  360, 2500,  "South", 0.2, "2000-3000m",
  250, 3500,  "North", 0, ">3000m",
  320, 3500,  "South", 0, ">3000m"
) %>% 
  mutate(label = if_else(y < 3000, sprintf("%+.1f%%", value*49/10), "?"),
         value_size = if_else(y < 3000, abs(value), 0.1),
         label_html = if_else(y > 3000, 
                              div_text_color(label, "#252525"),
                              if_else(value < 0, 
                                      div_text_color(label, "#d7301f"),
                                      div_text_color(label, "#377eb8"))))

div_text_color <- function(text, color){
  paste0("<div style='font-size:14px;color:", color, ";'>", text, "</div>")
}


x <- c("Elevation", "Change")
y <- sprintf("{point.%s:s}", c("elev_range", "label"))

tltip <- tooltip_table(x, y)

tltip_position <- JS("
function (labelWidth, labelHeight, point) { 
    return { 
        x: point.plotX + labelWidth / 2 + 20,
        y: point.plotY + labelHeight / 2 
    };
}
")


tltip_position <- JS("
function(labelWidth, labelHeight, point) {
        var tooltipX = point.plotX - 20;
        var tooltipY = point.plotY;
        return {
            x: tooltipX,
            y: tooltipY
        };
    }
")



highchart() %>% 
  # hc_add_theme(hc_theme_bloom()) %>% 
  hc_add_theme(hc_theme_hcrt()) %>% 
  hc_add_series(tbl_elev_sub,
                "line",
                hcaes(ii, elev_scaled),
                name = "Mountain",
                enableMouseTracking = F,
                showInLegend = F,
                states = list(inactive = list(opacity = 1))) %>% 
  hc_add_series(tbl_djf_rel,
                "scatter",
                hcaes(x, y, group = ns, size = value_size),
                dataLabels = list(enabled = T,
                                  align = "left",
                                  x = 20,
                                  useHTML = T,
                                  format = "{point.label_html}")) %>% 
  hc_colors(cols) %>%
  hc_legend(align = "center",
            verticalAlign = "top",
            x = -30) %>%
  hc_tooltip(useHTML = T,
             shape = "callout", # or rect
             pointFormat = tltip,
             headerFormat = "") %>% 
  hc_yAxis(title = list(text = "Elevation",
                        rotation = 0,
                        align = "high",
                        offset = 10,
                        y = 10),
           # breaks = c(0:3*1000),
           labels = list(format = "{value:f} m"),
           min = 0, max = 4700,
           # tickAmount = 2,
           tickInterval = 1000,
           endOnTick = F) %>% 
  hc_xAxis(visible = F) %>% 
  hc_title(text = "Change in average winter snow depth 1971-2019") %>% 
  hc_subtitle(text = "Winter is December - February, changes averaged for 1000 m elevation bands") %>% 
  hc_caption(text = strwrap(width = 1e6, 
                            "Changes are determined from linear regressions over the whole period, 
                            and averaged over all stations in 1000 m elevation bands. 
                            Over 3000 m, no stations with such long records exist.")) 




# mountain elev background (echarts) ------------------------------------------------

tbl_elev_sub %>% 
  e_chart(ii) %>% 
  e_line(elev_scaled)




# EOF ---------------------------------------------------------------------





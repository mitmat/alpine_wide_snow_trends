
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
                                     elev_rollmean)) %>% 
  dplyr::mutate(elev_scaled = ifelse(elev_scaled < 4000 & ii < 280,
                                     scales::rescale(elev_scaled, to = c(-100, 4000)),
                                     elev_scaled))



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



# ts plot -----------------------------------------------------------------


dat_plot_ts_mean[variable == "meanHS_DJF"] %>% 
  ggplot(aes(year, mean_value, colour = ns_fct))+
  geom_line()+
  geom_smooth(method = lm)+
  facet_wrap(~elev_fct, ncol = 1, scales = "free_y")+
  theme_bw()



# crosstalk stn level trends ----------------------------------------------

cols_cluster <- setNames(scales::brewer_pal(palette = "Set1")(5),
                         c("NW", "NE", "North & high Alpine", "South & high Alpine", "SE"))

dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")

dat_plot_month <- dat_month_gls[term == "year0"] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_month[, est_low := estimate - 1.96 * std.error]
dat_plot_month[, est_high := estimate + 1.96 * std.error]
mitmatmisc::add_month_fct(dat_plot_month, 10)
dat_plot_month[, hilo := fct_collapse(cluster_fct, 
                                      high = c("South & high Alpine", "North & high Alpine"),
                                      low = c("NW", "NE", "SE"))]
dat_plot_month[, Trend := round(estimate*10, 2)]
dat_plot_month[, Region := cluster_fct]
ylim_common <- range(dat_plot_month$Elevation)

f_plot_month <- function(dat, mo){
  
  dat[month == mo] %>% 
    ggplot(aes(Trend, Elevation, colour = Region, label = Name))+
    geom_vline(xintercept = 0)+
    geom_point()+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    scale_color_manual("", values = cols_cluster)+
    ylim(ylim_common)+
    xlab(paste0("Linear trend in ", month.name[mo], " mean HS [cm per decade]"))+
    ylab("Elevation [m]")
  
}



ggplotly(f_plot_month(dat_plot_month, 11),
         tooltip = c("x", "y", "colour", "label"))



dat_meta_cluster <- readRDS(here::here("data/meta-with-cluster-01.rds"))
sf_meta <- st_as_sf(dat_meta_cluster,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)
sf_meta <- sf_meta %>% mutate(Region = cluster_fct)
load(here::here("data/viz_past.Rdata"))


leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  # addProviderTiles("Esri.WorldTopoMap", group = "Topomap") %>% 
  # addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>% 
  
  addCircleMarkers(data = sf_meta,
                   stroke = F,
                   fillOpacity = 0.8,
                   radius = 5,
                   color = ~ mv5_leaf_col(Region),
                   group = "In-situ snow depth regions")



## shared data -------------------------------------------------------------

dat_plot_month[, .(Trend, Elevation, Region, Name, month)]

dat_plot_month %>% 
  dcast(Name + Region + Elevation + Latitude + Longitude ~ month_fct, value.var = "Trend" ) -> dat_share


sf_share <- sf_meta %>%
  left_join(dat_plot_month %>% 
              dcast(Name + Region + Elevation ~ month_fct, value.var = "Trend"))

  
shared_trends <- SharedData$new(sf_share)
shared_trends <- SharedData$new(sf_share %>% filter(!is.na(Nov)), key = ~Name)
shared_trends <- SharedData$new(dat_share[!is.na(Nov)], key = ~Name)
# shared_trends <- SharedData$new(sf_share[sample(nrow(sf_share), 100), ])



bscols(
  leaflet(shared_trends) %>% 
    addTiles %>% 
    addCircleMarkers(stroke = F,
                     fillOpacity = 0.8,
                     radius = 5,
                     color = ~ mv5_leaf_col(Region),
                     group = "In-situ snow depth regions"),
  ggplotly(
    ggplot(shared_trends, aes(Nov, Elevation, colour = Region, label = Name))+
      geom_vline(xintercept = 0)+
      geom_point()+
      theme_bw()+
      theme(panel.grid.minor = element_blank())+
      scale_color_manual("", values = cols_cluster)+
      ylim(ylim_common)+
      # xlab(paste0("Linear trend in ", month.name[mo], " mean HS [cm per decade]"))+
      ylab("Elevation [m]")
  )
)


bscols(
  leaflet(shared_trends, height = 300) %>% 
    addTiles %>% 
    addCircleMarkers(stroke = F,
                     fillOpacity = 0.8,
                     radius = 5,
                     color = ~ mv5_leaf_col(Region),
                     group = "In-situ snow depth regions"),
  d3scatter::d3scatter(shared_trends, ~Nov, ~Elevation, ~Region, height = 300)
)


bscols(
  leaflet(shared_trends) %>% 
    addTiles %>% 
    addCircleMarkers(stroke = F,
                     fillOpacity = 0.8,
                     radius = 5,
                     color = ~ mv5_leaf_col(Region),
                     group = "In-situ snow depth regions"),
  plot_ly(data = shared_trends,
          type = "scatter",
          x = ~ Nov, y = ~ Elevation)
)

ggplotly(
  ggplot(shared_trends, aes(Nov, Elevation, colour = Region, label = Name))+
    geom_vline(xintercept = 0)+
    geom_point()+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+
    scale_color_manual("", values = cols_cluster)+
    ylim(ylim_common)+
    # xlab(paste0("Linear trend in ", month.name[mo], " mean HS [cm per decade]"))+
    ylab("Elevation [m]")
) %>% 
  highlight(on = "plotly_selected", off = "plotly_relayout")





# legend clim plot --------------------------------------------------------


leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topomap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>% 

  addCircleMarkers(data = sf_meta,
                   stroke = F,
                   fillOpacity = 0.8,
                   radius = 5,
                   color = ~ leaf_col(Region),
                   group = "In-situ snow depth regions") %>% 
  addLegendFactor(pal = leaf_col, 
            values = sf_meta$Region,
            shape = "line",
            # width = 20, height = 20,
            opacity = 0.8,
            title = "In-situ snow depth regions",
            position = "bottomleft",
            group = "In-situ snow depth regions")


# legend for ts plot ------------------------------------------------------



sf_meta_ts <- sf_meta_ts %>% 
  mutate(col = cols_cluster[cluster_fct],
         icon = if_else(stn_long == "region",
                        "plus",
                        "asterisk"))

my_icons <- awesomeIcons(icon = sf_meta_ts$icon,
                          markerColor = sf_meta_ts$color,
                          library = "glyphicon")


sf_meta_ts %>% 
  leaflet() %>% 
  addProviderTiles("Esri.WorldTopoMap") %>% 
  addAwesomeMarkers(icon = my_icons)





# numPal <- colorNumeric('viridis', quakes$mag)
symbols <- makeSizeIcons(values = sf_meta_ts$Elevation,
                         shape = 'plus',
                         pal = leaf_col,
                         color = 'black',
                         colorValues = sf_meta_ts$cluster_fct,
                         baseSize = 10,
                         opacity = .5)
# symbols <- makeSymbolIcons(color = )

sf_meta_ts %>% 
  leaflet()


dat_meta_clust[, stn_long := ifelse(Name %in% stn_trend, "trend", "region")]
dat_meta_clust[, leaf_opac := ifelse(Name %in% stn_trend, 0.8, 0.4)]
dat_meta_clust[, leaf_rad := ifelse(Name %in% stn_trend, 6, 4)]

leaflet() %>% 
  addProviderTiles("CartoDB.Positron", group = "CartoDB") %>% 
  addProviderTiles("Esri.WorldTopoMap", group = "Topomap") %>% 
  addProviderTiles("Esri.WorldImagery", group = "WorldImagery") %>% 
  
  addLayersControl(baseGroups = c("CartoDB", "Topomap", "WorldImagery")) %>% 

  addCircleMarkers(data = sf_meta_ts %>% filter(stn_long == "region"),
                   color = ~leaf_col(cluster_fct),
                   stroke = F, 
                   radius = 5,
                   fillOpacity = 0.4,
                   popup = ~popup_html) %>% 
  addCircleMarkers(data = sf_meta_ts %>% filter(stn_long == "trend"),
                   color = ~leaf_col(cluster_fct),
                   stroke = T, 
                   weight = 2,
                   radius = 5,
                   opacity = 0.8,
                   fillOpacity = 0.4,
                   popup = ~popup_html) %>% 
  addLegendCustom(colors = "gray",
                  labels = c("trend", "region"),
                  sizes = 10,
                  shapes = "circle",
                  borders = c("white", "black"))

# set legend features
colors <- c("red", "white", "blue", "white", "blue", "red")
labels <- c("filled_square", "empty_square", "big_square", "empty_circle", "filled_circle", "big_circle")
sizes <- c(10, 20, 30, 10, 20, 30)
shapes <- c("square", "square", "square", "circle", "circle", "circle")
borders <- c("red", "blue", "black", "blue", "blue", "black")

addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 0.5){
  
  make_shapes <- function(colors, sizes, borders, shapes) {
    shapes <- gsub("circle", "50%", shapes)
    shapes <- gsub("square", "0%", shapes)
    paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
  }
  make_labels <- function(sizes, labels) {
    paste0("<div style='display: inline-block;height: ", 
           sizes, "px;margin-top: 4px;line-height: ", 
           sizes, "px;'>", labels, "</div>")
  }
  
  legend_colors <- make_shapes(colors, sizes, borders, shapes)
  legend_labels <- make_labels(sizes, labels)
  
  return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity))
}


# future summary ----------------------------------------------------------

library(data.table)
library(forcats)
load("data/future-summary-elev-500m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600,
                       .(scd = mean(scd)),
                       .(elev_f, elev, experiment, period, fp)]


# dat_ens_mean <- dat_bc[elev > 200 & elev < 3600,
#                        .(scd = mean(snc)*365),
#                        .(elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n-\n2020" = "2001-2020",
                                      "2041\n-\n2070" = "2041-2070",
                                      "2071\n-\n2100" = "2071-2100")]


dat_ens_mean %>% 
  ggplot(aes(period, scd, colour = experiment))+
  geom_point()+
  geom_line(aes(group = paste0(elev, experiment)))+
  theme_bw()

dat_ens_mean %>% 
  ggplot(aes(period, scd, colour = experiment))+
  geom_point()+
  geom_line(aes(group = paste0(elev, experiment)))+
  theme_bw()+
  facet_grid(elev_f ~ . , scales = "free_y", space = "free_y")


dat_ens_mean %>% 
  ggplot(aes(period_f, scd, colour = experiment))+
  geom_point()+
  geom_line(aes(group = paste0(elev, experiment)))+
  theme_bw()+
  facet_grid(. ~ elev)



dat_ens_mean %>% 
  ggplot(aes(scd, fct_rev(period), colour = experiment))+
  geom_point()+
  geom_line(aes(group = paste0(elev, experiment)))+
  theme_bw()+
  facet_grid(fct_rev(elev_f) ~ .)



dat_ens_mean %>% 
  ggplot(aes(period_f, scd, colour = experiment))+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0:5*60, ymax = 1:6*60 - 30, alpha = 0.1)+ # 1 month
  geom_hline(yintercept = 0:12*30, alpha = 0.2)+ # 1 month
  geom_point()+
  geom_line(aes(group = paste0(elev, experiment)))+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(expand = c(0,0), limits = c(0,360))+
  facet_grid(. ~ elev)+
  cowplot::theme_cowplot()


dat_ens_mean[period != "2041-2070"] %>% 
  dcast(elev ~ experiment + fp, value.var = "scd") -> dat_segment

dat_ens_mean[period != "2041-2070"] %>% 
  ggplot(aes(period_f, scd, colour = experiment))+
  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0:5*60, ymax = 1:6*60 - 30, alpha = 0.1)+ # 1 month
  geom_hline(yintercept = 0:12*30, alpha = 0.2)+ # 1 month
  geom_segment(data = dat_segment, inherit.aes = F,
               aes(x = "2071\n-\n2100", xend = "2071\n-\n2100", 
                   y = scd, yend = scd, alpha = experiment))+
  geom_point()+
  geom_point(data = dat_ens_mean[period == "2001-2020"], colour = "black")+
  geom_line(aes(group = paste0(elev, experiment)))+
  scale_color_brewer(palette = "Set1")+
  scale_y_continuous(expand = c(0,0), limits = c(0,360))+
  facet_grid(. ~ elev)+
  cowplot::theme_cowplot()+
  xlab(NULL)+
  ylab("Snow cover duration [days]")



# future summary lollipop -------------------------------------------------

dat_ens_mean[period != "2041-2070"] %>% 
  dcast(elev ~ experiment + fp, value.var = "scd") -> dat_lollipop

dat_lollipop[, elev_fct := fct_inorder(paste0(elev, " m"))]

rect_width <- 0.2

# cols <- setNames(c("#3182bd", "#de2d26", "#fee090", grey(0.7)),
#                  c("rcp26", "rcp85", "loss", "anno_month"))
cols <- setNames(c("#377eb8", "#e41a1c", "#ff7f00", grey(0.7)),
                 c("rcp26", "rcp85", "loss", "anno_month"))


dat_lollipop %>% 
  ggplot()+
  
  geom_vline(xintercept = 0:12*30, colour = cols["anno_month"])+ # 1 month
  
  geom_vline(aes(xintercept = rcp26_past))+
  
  geom_point(aes(x = rcp26_future, y = 3), colour = cols["rcp26"])+
  geom_segment(aes(x = rcp26_future, xend = rcp26_past, y = 3, yend = 3), colour = cols["rcp26"])+
  
  # geom_errorbarh(aes(xmin = rcp85_future, xmax = rcp26_future, y = 2))+
  geom_rect(aes(xmin = rcp85_future, xmax = rcp26_future,
                ymin = 1 - rect_width, ymax = 1 + rect_width),
            fill = cols["loss"])+
  
  geom_point(aes(x = rcp85_future, y = 2), colour = cols["rcp85"])+
  geom_segment(aes(x = rcp85_future, xend = rcp26_past, y = 2, yend = 2), colour = cols["rcp85"])+

  scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = 0:3*90)+
  scale_y_continuous(breaks = 1:3, labels = c("RCP2.6 (1.5 - 2°C)",
                                              "loss due to delay / no action",
                                              "RCP8.5 (4 - 5°C)"),
                     limits = c(0,4))+
  facet_grid(elev_fct ~ ., as.table = F, switch = "y")+
  cowplot::theme_cowplot()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))+
  
  ylab(NULL)+
  xlab("Snow cover duration (SCD) [days]")+
  
  geom_text(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 315, y = 1, label = "(~1 month)", colour = cols["anno_month"])+
  geom_text(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 315, y = 3, label = "30 days", colour = cols["anno_month"])+
  geom_segment(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))),
               x = 300, xend = 330, y = 2, yend = 2, colour = cols["anno_month"],
               arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "in")))+
  
  geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 180, y = 0, hjust = 0, vjust = 0,
            label = "recent SCD")+
  geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 150, y = 3, hjust = 1, vjust = 0.5, colour = cols["rcp26"],
            label = "future SCD with 1.5-2°C warming")+
  geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 105, y = 2, hjust = 1, vjust = 0.5, colour = cols["rcp85"],
            label = "future SCD with 4-5°C warming")+
  geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
            x = 105, y = 1, hjust = 1, vjust = 0.5, colour = cols["loss"],
            label = "SCD saved through climate action")+
  
  ggtitle("Recent (2001-2020) and future (2071-2100) snow cover duration by elevation")


  # annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0:5*60, ymax = 1:6*60 - 30, alpha = 0.1)+ # 1 month
  



  

    

# gauges with highcharts --------------------------------------------------

library(highcharter)

my_gauge <- function(val, min, max){
  
  col <- "#3182bd"
  
highchart() %>%
  hc_chart(type = "solidgauge") %>%
  hc_pane(
    startAngle = -90,
    endAngle = 90,
    background = list(
      outerRadius = '100%',
      innerRadius = '60%',
      shape = "arc"
    )
  ) %>%
  hc_tooltip(enabled = FALSE) %>%
  hc_yAxis(
    # stops = list_parse2(col_stops),
    minColor = col,
    maxColor = col,
    lineWidth = 0,
    minorTickWidth = 0,
    tickAmount = 2,
    min = min,
    max = max,
    labels = list(y = 26, style = list(fontSize = "22px"),
                  format = "{text} d")
  ) %>%
  hc_add_series(
    data = val,
    dataLabels = list(
      format = paste0(-min+val, " d"),
      y = -50,
      borderWidth = 0,
      useHTML = TRUE,
      style = list(fontSize = "30px", color = col)
    )
  ) %>% 
  hc_size(height = 300)

}

my_gauge(min = dat_gauge[elev == 1500, rcp85],
         max = 0,
         val = dat_gauge[elev == 1500, rcp26])



# xrange ------------------------------------------------------------------


library(lubridate)

N <- 7
set.seed(1234)

df <- tibble(
  start = Sys.Date() + months(sample(10:20, size = N)),
  end = start + months(sample(1:3, size = N, replace = TRUE)),
  cat = rep(1:5, length.out = N) - 1,
  progress = round(stats::runif(N), 1)
)

df <- mutate_if(df, is.Date, datetime_to_timestamp)

hchart(
  df,
  "xrange",
  hcaes(x = start, x2 = end, y = cat, partialFill = progress),
  dataLabels = list(enabled = TRUE)
) %>% 
  hc_xAxis(
    title = FALSE,
    type = "datetime"
  ) %>% 
  hc_yAxis(
    title = FALSE,
    categories = c("Prototyping", "Development", "Testing", "Validation", "Modelling")
  )



# future summary: dotplot --------------------------------------------------

dat_lollipop[, .(elev,
                 v1 = rcp85_future, 
                 v2 = rcp26_future - rcp85_future,
                 v3 = rcp26_past - rcp26_future)] %>% 
  melt(id.vars = "elev",
       measure.vars = paste0("v", 1:3)) %>% 
  .[,
    .(i_dot_grp = 1:round(value)),
    .(elev, variable)] -> dat_dp


dat_dp[, i_dot := 1:.N - 1, elev]
dat_dp[, xx := i_dot %% 10]
dat_dp[, yy := i_dot %/% 10]

# dat_dp %>% 
#   ggplot(aes(xx, y = 100, fill = variable))+
#   geom_dotplot(stackgroups = T, binwidth = 1, method = "histodot")
#   # scale_y_continuous(NULL, breaks = NULL)


# cols <- setNames(c("#377eb8", "#e41a1c", "#ff7f00", grey(0.7)),
#                  c("ss", "v2", "v3", "v1"))

# cols <- setNames(c("#a6cee3", "#fdbf6f", "#b2df8a"),
#                  c("v1", "v2", "v3"))

cols <- setNames(c("#1f78b4", "#ff7f00", "#33a02c"),
                 c("v1", "v2", "v3"))

cols <- setNames(c(grey(0.8), "#ff7f00", "#33a02c"),
                 c("v1", "v2", "v3"))

dat_dp %>% 
  ggplot(aes(xx, yy, colour = variable))+
  geom_point(shape="\u2744", size = 5)+
  scale_color_manual(values = cols)+
  facet_grid(. ~ elev, switch = "x")+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside")+
  xlab("Elevation [m]")+
  scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  ggtitle("Days with snow on ground")


# legend plot



dat_dp2 <- rbind(dat_dp[elev == 1500, 
                        .(ff = "present", variable = "v1", xx, yy)],
                 dat_dp[elev == 1500, 
                        .(ff = "future1", variable = ifelse(variable == "v3", "v3", "v1"), xx, yy)],
                 dat_dp[elev == 1500 & variable != "v3", 
                        .(ff = "future2", variable, xx, yy)])
dat_dp2[, ff := fct_inorder(ff)]

dat_dp2 %>% 
  ggplot(aes(xx, yy, colour = variable))+
  geom_point(shape="\u2744", size = 5)+
  scale_color_manual(values = cols)+
  # scale_size_manual(values = c(3, 5, 5))+
  facet_grid(. ~ ff)+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside")
  
dat_dp3 <- rbind(dat_dp[elev == 1500, 
                        .(ff = "present", variable = "v1", xx, yy)],
                 dat_dp[elev == 1500& variable != "v1", 
                        .(ff = "future1" , variable, xx, yy)])

dat_dp3 %>% 
  ggplot(aes(xx, yy, colour = variable, size = variable))+
  geom_point(shape="\u2744")+
  scale_color_manual(values = cols)+
  scale_size_manual(values = c(5, 3, 3)*2)+
  # facet_grid(. ~ ff)+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside")


cols <- setNames(c("#1f78b4", "#ff7f00", "#33a02c"),
                 c("v1", "v2", "v3"))


ggplot()+
  geom_tile(data = dat_dp[elev == 1500],
            aes(xx, yy, fill = variable), 
            width = 1, height = 1, alpha = 0.5)+
  geom_point(data = dat_dp[elev == 1500], 
             aes(xx, yy, colour = variable == "v1"),
             shape = "\u2744", size = 8)+
  scale_fill_manual(values = cols)+
  scale_colour_manual(values = c("white", "white"))+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside")
  



## version 2 ---------------------------------------------------------------


cols <- setNames(c("#a6cee3", "#fdbf6f", "#b2df8a"),
                 c("v1", "v2", "v3"))

# cols <- setNames(c("#1f78b4", "#ff7f00", "#33a02c"),
#                  c("v1", "v2", "v3"))
# 
# cols <- setNames(c(grey(0.8), "#ff7f00", "#33a02c"),
#                  c("v1", "v2", "v3"))



gg <- dat_dp %>% 
  ggplot()+
  geom_tile(aes(xx, yy, fill = variable), 
            width = 1, height = 1)+
  geom_point(aes(xx, yy,),
             shape = "\u2744", size = 5, colour = "white")+
  scale_fill_manual(values = cols)+
  facet_grid(. ~ elev, switch = "x")+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    legend.position = "none")+
  xlab("Elevation [m]")+
  scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  ggtitle("Impact of global warming on snow in the Alps")
  
  # legend
  # geom_text(data = data.table(elev = 500, x = 0, y = 35, label = "Day(s) with snow on ground"), 
  #           aes(x, y, label = label), hjust = 0)



# annotate
library(cowplot)


gg_in_scd <-
data.table(xx = 1:3, yy = 1, variable = paste0("v", 1:3)) %>% 
  ggplot()+
  geom_tile(aes(xx, yy, fill = variable), 
            width = 1, height = 1)+
  geom_point(aes(xx, yy),
             shape = "\u2744", size = 5, colour = "white")+
  scale_fill_manual(values = cols, guide = "none")+
  theme_void()


ggdraw(gg)+
  draw_label("Day(s) with snow on ground", 0.05, 0.9, hjust = 0)+
  draw_plot(gg_in_scd, 0.25, 0.9, width = 0.15, height = 0.05)




## version 3 ---------------------------------------------------------------

# try with no facets -> makes annotation easier!

dat_dp %>% 
  ggplot()+
  geom_tile(aes(xx, yy, fill = variable), 
            width = 1, height = 1)+
  geom_point(aes(xx, yy,),
             shape = "\u2744", size = 5, colour = "white")+
  scale_fill_manual(values = cols, aesthetics = c("colour", "fill"))+
  facet_grid(. ~ elev, switch = "x")+
  cowplot::theme_cowplot()+
  theme(#axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.placement = "outside",
    legend.position = "none")+
  xlab("Elevation [m]")+
  scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  ggtitle("Impact of global warming on snow in the Alps")+
  
  geom_curve(data = data.table(elev = 2000, variable = "v3", 
                               x = 9, xend = 7, y = 25, yend = 17),
             aes(x, y, xend = xend, yend = yend, colour = variable),
             curvature = -0.3,
             arrow = arrow(type = "closed", length = unit(0.25, "cm")))



# future summary with data ------------------------------------------------

library(data.table)
library(forcats)
library(ggplot2)
load("data/future-summary-elev-500m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600,
                       .(scd = mean(scd)),
                       .(elev_f, elev, experiment, period, fp)]


# dat_ens_mean <- dat_bc[elev > 200 & elev < 3600,
#                        .(scd = mean(snc)*365),
#                        .(elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n-\n2020" = "2001-2020",
                                      "2041\n-\n2070" = "2041-2070",
                                      "2071\n-\n2100" = "2071-2100")]

dat_ens_mean[period != "2041-2070"] %>% 
  dcast(elev ~ experiment + fp, value.var = "scd") -> dat_lollipop

dat_lollipop[, elev_fct := fct_inorder(paste0(elev, " m"))]

dat_lollipop[, .(elev,
                 v1 = rcp85_future, 
                 v2 = rcp26_future - rcp85_future,
                 v3 = rcp26_past - rcp26_future)] %>% 
  melt(id.vars = "elev",
       measure.vars = paste0("v", 1:3)) %>% 
  .[,
    .(i_dot_grp = 1:round(value)),
    .(elev, variable)] -> dat_dp


dat_dp[, i_dot := 1:.N - 1, elev]
dat_dp[, xx := i_dot %% 10]
dat_dp[, yy := i_dot %/% 10]

# cols <- setNames(c("#377eb8", "#e41a1c", "#ff7f00", grey(0.7)),
#                  c("ss", "v2", "v3", "v1"))

# cols <- setNames(c("#a6cee3", "#fdbf6f", "#b2df8a"),
#                  c("v1", "v2", "v3"))

cols <- setNames(c("#1f78b4", "#ff7f00", "#33a02c"),
                 c("v1", "v2", "v3"))

cols <- setNames(c(grey(0.8), "#ff7f00", "#33a02c"),
                 c("v1", "v2", "v3"))



## plot --------------------------------------------------------------------



# try with no facets -> makes annotation easier!

x_rat <- 80
elev_plot <- c(500, 1500, 2500, 3500)

dat_dp[, xx_plot := elev + x_rat*xx - 10*x_rat/2]

dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8))+ # 1 month
  
  geom_point(shape = "\u2744", size = 5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = 3, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = 4, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  # scale_fill_manual(values = cols, aesthetics = c("colour", "fill"))+
  cowplot::theme_cowplot()+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 36.1), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Days with snow on ground in the Alps",
          "Impact of global warming and climate action for end of century (2071-2100) snow cover")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 3000, ymin = 26.9, ymax = 36.1, 
           colour = "white", fill = "white")+
  annotate("point", 100, 34.5, shape = "\u2744", size = 5, colour = "#9ecae1")+
  annotate("text", 150, 34.5, hjust = 0, 
           label = "Day with snow on ground, recent (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 31.5, shape = 4, size = 3, colour = "black")+
  annotate("text", 150, 31.5, hjust = 0, vjust = 0.5,
           label = "Future loss if global warming is 1.5-2°C (commited loss)", colour = "black")+
  
  annotate("point", 100, 28.5, shape = 1, size = 4, colour = "#e6550d")+
  annotate("text", 100, 28.5, label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  annotate("text", 150, 28.5, hjust = 0, vjust = 0.5,
           label = "Extra loss if global warming is 4-5°C (can be saved with climate action)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 1000, xend = 1000, y = 18, yend = 15, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "in")))+
  annotate("text", x = 1000, y = 16.5, hjust = -0.1, label = "(~1 month)", colour = grey(0.8))+
  annotate("text", x = 1000, y = 16.5, hjust = 1.2, label = "30 days", colour = grey(0.8))
  


# future summary country --------------------------------------------------


library(ggplot2)
library(magrittr)
library(data.table)
library(forcats)
load("data/future-summary-country-elev-500m.rda")
# load("data/future-summary-country-elev-200m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

# remove some countries
country_remove <- c("Hungary", "Liechtenstein", "San Marino", "Slovakia")

# dat_ens_mean <- dat_ds[elev > 200 & elev < 3600 & ! country %in% country_remove,
#                        .(scd = mean(scd)),
#                        .(country, elev_f, elev, experiment, period, fp)]

dat_ens_mean <- dat_bc[elev > 200 & elev < 3600 & ! country %in% country_remove,
                       .(scd = mean(snc)*365),
                       .(country, elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n-\n2020" = "2001-2020",
                                      "2041\n-\n2070" = "2041-2070",
                                      "2071\n-\n2100" = "2071-2100")]


dat_ens_mean[period != "2041-2070"] %>% 
  dcast(country + elev ~ experiment + fp, value.var = "scd") -> dat_lollipop

dat_lollipop[, elev_fct := fct_inorder(paste0(elev, " m"))]
dat_lollipop[, country_fct := factor(country)]

rect_width <- 0.2

# cols <- setNames(c("#3182bd", "#de2d26", "#fee090", grey(0.7)),
#                  c("rcp26", "rcp85", "loss", "anno_month"))
cols <- setNames(c("#377eb8", "#e41a1c", "#ff7f00", grey(0.7)),
                 c("rcp26", "rcp85", "loss", "anno_month"))


pdf("fig/future-country/lollipop.pdf",
    width = 6, height = 4)

for(i_elev in unique(dat_lollipop$elev)){
  
  
  gg <- dat_lollipop[elev == i_elev] %>% 
    ggplot()+
    
    geom_vline(xintercept = 0:12*30, colour = cols["anno_month"])+ # 1 month
    
    geom_vline(aes(xintercept = rcp26_past))+
    
    geom_point(aes(x = rcp26_future, y = 3), colour = cols["rcp26"])+
    geom_segment(aes(x = rcp26_future, xend = rcp26_past, y = 3, yend = 3), colour = cols["rcp26"])+
    
    # geom_errorbarh(aes(xmin = rcp85_future, xmax = rcp26_future, y = 2))+
    geom_rect(aes(xmin = rcp85_future, xmax = rcp26_future,
                  ymin = 1 - rect_width, ymax = 1 + rect_width),
              fill = cols["loss"])+
    
    geom_point(aes(x = rcp85_future, y = 2), colour = cols["rcp85"])+
    geom_segment(aes(x = rcp85_future, xend = rcp26_past, y = 2, yend = 2), colour = cols["rcp85"])+
    
    scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = 0:3*90)+
    scale_y_continuous(breaks = 1:3, labels = c("RCP2.6 (1.5 - 2°C)",
                                                "loss due to delay / no action",
                                                "RCP8.5 (4 - 5°C)"),
                       limits = c(0,4))+
    facet_grid(country_fct ~ ., as.table = T, switch = "y", drop = F)+
    cowplot::theme_cowplot()+
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0))+
    
    ylab(NULL)+
    xlab("Snow cover duration (SCD) [days]")+
    ggtitle(paste0(i_elev, " m"))
  
  print(gg)
  
}
dev.off()


dat_lollipop %>% 
  ggplot()+
  
  geom_vline(xintercept = 0:12*30, colour = cols["anno_month"])+ # 1 month
  
  geom_vline(aes(xintercept = rcp26_past))+
  
  geom_point(aes(x = rcp26_future, y = 3), colour = cols["rcp26"])+
  geom_segment(aes(x = rcp26_future, xend = rcp26_past, y = 3, yend = 3), colour = cols["rcp26"])+
  
  # geom_errorbarh(aes(xmin = rcp85_future, xmax = rcp26_future, y = 2))+
  geom_rect(aes(xmin = rcp85_future, xmax = rcp26_future,
                ymin = 1 - rect_width, ymax = 1 + rect_width),
            fill = cols["loss"])+
  
  geom_point(aes(x = rcp85_future, y = 2), colour = cols["rcp85"])+
  geom_segment(aes(x = rcp85_future, xend = rcp26_past, y = 2, yend = 2), colour = cols["rcp85"])+
  
  scale_x_continuous(limits = c(0, 360), expand = c(0, 0), breaks = 0:3*90)+
  scale_y_continuous(breaks = 1:3, labels = c("RCP2.6 (1.5 - 2°C)",
                                              "loss due to delay / no action",
                                              "RCP8.5 (4 - 5°C)"),
                     limits = c(0,4))+
  facet_grid(elev_fct ~ country, as.table = F, switch = "y")+
  cowplot::theme_cowplot()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))+
  
  ylab(NULL)+
  xlab("Snow cover duration (SCD) [days]")
  
  # annotations
  # geom_text(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 315, y = 1, label = "(~1 month)", colour = cols["anno_month"])+
  # geom_text(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 315, y = 3, label = "30 days", colour = cols["anno_month"])+
  # geom_segment(data = data.frame(elev_fct = factor("1000 m", levels = levels(dat_lollipop$elev_fct))),
  #              x = 300, xend = 330, y = 2, yend = 2, colour = cols["anno_month"],
  #              arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "in")))+
  # 
  # geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 180, y = 0, hjust = 0, vjust = 0,
  #           label = "recent SCD")+
  # geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 150, y = 3, hjust = 1, vjust = 0.5, colour = cols["rcp26"],
  #           label = "future SCD with 1.5-2°C warming")+
  # geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 105, y = 2, hjust = 1, vjust = 0.5, colour = cols["rcp85"],
  #           label = "future SCD with 4-5°C warming")+
  # geom_text(data = data.frame(elev_fct = factor("2000 m", levels = levels(dat_lollipop$elev_fct))), 
  #           x = 105, y = 1, hjust = 1, vjust = 0.5, colour = cols["loss"],
  #           label = "SCD saved through climate action")+
  # 
  # ggtitle("Recent (2001-2020) and future (2071-2100) snow cover duration by elevation")





# EOF ---------------------------------------------------------------------





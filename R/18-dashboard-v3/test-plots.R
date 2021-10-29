
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


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
# library(foreach)
library(scico)
library(sf)



# snow cluster
dat_meta_cluster <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
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
        legend.position = "bottom",
        # legend.title = element_blank(),
        legend.background = element_rect(colour = "grey"),
        legend.direction = "horizontal",
        legend.box.just = "right")+
  scale_shape("Snow depth clusters")+
  scale_color_brewer("Snow depth clusters", palette = "Set1")+
  guides(color = guide_legend(override.aes = list(size = 4)))


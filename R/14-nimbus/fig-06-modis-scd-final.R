# get MODIS data


# library(raster)
library(stars)
library(data.table)
library(lubridate)
library(magrittr)
library(fs)
library(ggplot2)
library(forcats)
library(scico)
library(graticule)


# map: clim with graticule --------------------------------------------------------------------

rs_clim <- read_stars("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif")

prj <- st_crs(rs_clim)$proj4string

lons <- seq(10, 12, by = 2)
lats <- seq(46, 48, by = 1)
xl <- range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
grat <- graticule(lons, lats, proj = prj, xlim = xl, ylim = yl)
labs <- graticule_labels(lons, lats, xline = min(xl), yline = max(yl), proj = prj)


gg_clim <-
  ggplot()+
  geom_sf(data = st_as_sf(grat))+
  geom_stars(data = rs_clim, downsample = c(1))+
  scale_fill_scico("Durata media della copertura nevosa sul periodo 2000-2019 [giorni]",
                   palette = "devon", na.value = NA, direction = 1)+
  coord_sf(expand = F)+
  theme_minimal(12)+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  theme(legend.position = "bottom")

ggsave(gg_clim,
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig_final/Figura_9.jpg",
       width = 8*2.54,
       height = 5*2.54,
       units = "cm")




library(stars)
library(ggplot2)
library(scico)

rs_scd <- read_stars("data-raw/2000-10-01_2019-09-30.tif")
rs_scd_anom <- read_stars("data-raw/2000-10-01_2019-09-30_bin100m.tif")


# ds_stars <- 10 # for template
ds_stars <- 1 # for production 2-5



##graticules 

prj <- st_crs(rs_scd)$proj4string
lons <- seq(10, 12, by = 2)
lats <- seq(46, 48, by = 1)
xl <- range(lons) + c(-0.4, 0.4)
yl <- range(lats) + c(-0.4, 0.4)
grat <- graticule::graticule(lons, lats, proj = prj, xlim = xl, ylim = yl)
# labs <- graticule_labels(lons, lats, xline = min(xl), yline = max(yl), proj = prj)




# plots

gg_scd <- ggplot()+
  geom_sf(data = st_as_sf(grat), colour = NA)+
  geom_stars(data = rs_scd, downsample = ds_stars)+
  scico::scale_fill_scico("[Tage]", limits = c(0, 366),
                          palette = "davos", na.value = NA, direction = 1)+
  coord_sf(expand = F)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  ggtitle("Mittlere Anzahl Tage mit Schneebedeckung (2000-2019)")

ggsave(gg_scd,
       filename = paste0("~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/fig-fabio/SCD_ds", ds_stars, ".png"),
       width = 10, height = 5,
       dpi = 1200)

ggsave(gg_scd,
       filename = paste0("~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/fig-fabio/SCD_ds", ds_stars, ".pdf"),
       width = 10, height = 5)

gg_anom <- ggplot()+
  geom_sf(data = st_as_sf(grat), colour = NA)+
  geom_stars(data = rs_scd_anom, downsample = ds_stars)+
  scico::scale_fill_scico("[Tage]", 
                          palette = "vik", na.value = NA, direction = -1,
                          rescaler = scales::rescale_mid)+
  coord_sf(expand = F)+
  theme_minimal()+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  ggtitle("Abweichung der Schneebedeckungsdauer nach Höhenstufe",
          "Pixel Schneebedeckungsdauer minus dem räumlichen Mittelwert bei gleicher Höhe (+-50m)")


ggsave(gg_anom,
       filename = paste0("~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/fig-fabio/SCD_anom_ds", ds_stars, ".png"),
       width = 10, height = 5,
       dpi = 1200)

ggsave(gg_anom,
       filename = paste0("~/alps-snow/ALPINE_WIDE_SNOW/09_EXPORT/fig-fabio/SCD_anom_ds", ds_stars, ".pdf"),
       width = 10, height = 5)
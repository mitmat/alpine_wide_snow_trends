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

all_files <- dir_ls("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/annual_SCD/")
#remove incomplete
all_files <- all_files[2:20]
all_files %>% path_file

all_files %>% 
  path_file %>% 
  substr(1,4) -> along_year

rs_all <- read_stars(all_files,
                     along = list(hydro_year = along_year))
rs_all
# plot(rs_all)

all_files %>% 
  path_file %>% 
  path_ext_remove() %>% 
  stringr::str_split("_", simplify = T) -> files_dates
new_names <- paste0(substr(files_dates[, 1], 1, 7),
                    "...",
                    substr(files_dates[, 2], 1, 7))

rs_all2 <- rs_all %>% st_set_dimensions("hydro_year", value = new_names)


# map: year ---------------------------------------------------------------



gg_all <-
  ggplot()+
  geom_stars(data = rs_all2, downsample = c(10,10,1))+
  scale_fill_scico("Durata della copertura nevosa [giorni]",
                   palette = "devon", na.value = NA, direction = 1)+
  facet_wrap(~hydro_year)+
  coord_sf(expand = F)+
  theme_void()+
  theme(legend.position = "bottom")

ggsave(gg_all,
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/map_modis_year.png",
       width = 12,
       height = 8)



# map: clim --------------------------------------------------------------------

# rs_clim <- st_apply(rs_all, c("x", "y"), mean)
rs_clim <- read_stars("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif")

# gg_clim <-
  ggplot()+
  geom_stars(data = rs_clim, downsample = c(100))+
  scale_fill_scico("Durata media della copertura nevosa sul periodo 2000-2019 [giorni]",
                   palette = "devon", na.value = NA, direction = 1)+
  coord_sf(expand = F, label_graticule = "NE")+
  # theme_void()+
  theme(legend.position = "bottom")

ggsave(gg_clim,
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/map_modis_clim.png",
       width = 8,
       height = 5)



# map: clim with graticules --------------------------------------------------------------------



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
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/map_modis_clim_grat.png",
       width = 8,
       height = 5)


# figure for Luca Panziera

library(sf)
library(ggplot2)
library(dplyr)

dat_meta <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/06_SEASONAL/indices/meta_all.rds")
dat_djf <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/06_SEASONAL/indices/meanHS_DJF.rds")


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)


dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/01_submission/rds/meta-with-cluster-01.rds")
load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/01_submission/rds/data4corr-01-merged.rda")

# 
# # need to adjust by lapse rate!
# dat_elev_eobs <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-02-elev.rds")
# 
# dat_hs_apgd_eobs2 <- merge(dat_hs_apgd_eobs, dat_elev_eobs, by = "Name") %>% 
#   merge(dat_meta_clust)
# dat_hs_apgd_eobs2[, tmean_lapse := tmean + (elev_eobs - Elevation) * 6.4 / 1000]


dat_elev_uerra <-  readRDS("~/alps-snow/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/uerra-02-elev.rds")

dat_hs_uerra_full %>% 
  merge(dat_meta_clust) %>% 
  merge(dat_elev_uerra) -> dat_hs_uerra_full2

dat_hs_uerra_full2[, tmean_lapse := tmean + (elev_uerra - Elevation) * 6.4 / 1000]



mitmatmisc::add_hydro_year(dat_hs_uerra_full2)
mitmatmisc::add_season_fct(dat_hs_uerra_full2)

dat_hs_uerra_full2[hydro_year %in% c(1981:2010),
                   .(HS = mean(HS),
                     tmean_lapse = mean(tmean_lapse),
                     prec = sum(prec),
                     nn = .N),
                   .(Name, hydro_year, season)] %>% 
  .[nn == 3] %>% 
  .[, 
    .(HS = mean(HS),
      tmean_lapse = mean(tmean_lapse),
      prec = mean(prec),
      nn = .N),
    .(Name, season)] %>% 
  .[nn == 30] -> dat_clim




# plot --------------------------------------------------------------------

dat_plot <- merge(dat_clim[!is.na(HS)], dat_meta_clust, by = "Name")

sf_plot <- st_as_sf(dat_plot,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)

sf_plot %>% 
  dplyr::filter(season == "DJF") %>% 
  # dplyr::filter(Name != "Zugspitze") %>% 
  ggplot(aes(color = HS))+
  geom_sf()+
  theme_bw()+
  scale_color_viridis_c()
  # scale_color_viridis_c(trans = "log10")



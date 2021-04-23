# get MODIS data


library(raster)
# library(stars)
library(data.table)
library(lubridate)
library(magrittr)
library(fs)
library(ggplot2)
library(forcats)
library(scico)

rr_zone <- raster("data/zones-manual-v01.tif") 
# 1 = S, 2 = N
rr_elev <- raster("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/00_aux/eurac_modis_altitude_laea.tif")
rr_clim <- raster("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif")

dat <- data.table(icell = 1:ncell(rr_zone),
                  zone = rr_zone[],
                  elev = rr_elev[],
                  scd = rr_clim[])
dat[elev < -1000, elev := NA]

elev_breaks <- seq(-150, 4800, by = 100)

dat[, elev_fct := cut(elev, breaks = elev_breaks, dig.lab = 5)]


dat_summ <- dat[!is.na(scd),
                .(scd_mean = mean(scd),
                  scd_min = min(scd),
                  scd_max = max(scd),
                  scd_sd = sd(scd),
                  nn = .N),
                .(zone, elev_fct)]



# plot --------------------------------------------------------------------

elev_breaks_mid <- (elev_breaks[-50] + elev_breaks[-1])/2

dat_summ[, zone_fct := fct_recode(factor(zone), "Sud" = "1", "Nord" = "2")]
dat_summ[, elev_fct_num := elev_breaks_mid[as.numeric(elev_fct)]]


dat_summ %>% 
  # ggplot(aes(scd_mean, elev_fct_num, colour = zone_fct))+
  ggplot(aes(elev_fct_num, scd_mean, colour = fct_rev(zone_fct)))+
  geom_point()+
  # geom_linerange(aes(xmin = scd_mean - scd_sd, xmax = scd_mean + scd_sd))+
  geom_smooth(formula = y ~ s(x, k = 20), method = "gam", se = F)+
  scale_color_brewer("", palette = "Dark2", direction = 1)+
  theme_bw()+
  xlab("Quota [m]")+
  ylab("Giorni")+
  ggtitle("Durata media della copertura nevosa sul periodo 2000-2019")

ggsave(filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/modis_zone_clim_elev.png",
       width = 6, height = 4)

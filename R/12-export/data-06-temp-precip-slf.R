# 

library(data.table)
library(magrittr)
library(fs)



dat_apgd <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/apgd-01-series.rds")
dat_chelsa <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/chelsa-01-series.rds")
dat_eobs <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-01-series.rds")
dat_uerra <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/uerra-01-series.rds")

dat_elev_eobs <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-02-elev.rds")
dat_elev_uerra <- readRDS("~/projects/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/uerra-02-elev.rds")


dat_all <- dat_apgd[, .(Name, year, month, prec_apgd = prec)] %>% 
  merge(dat_chelsa[, .(Name, year, month, prec_chelsa = prec, tmean_chelsa = tmean)], all = T) %>% 
  merge(dat_eobs[, .(Name, year, month, tmean_eobs = tmean)], all = T) %>% 
  merge(dat_uerra[, .(Name, year, month, prec_mescan = prec, tmean_mescan = tmean)], all = T)

dat_elev_all <- dat_elev_eobs %>% 
  merge(dat_elev_uerra[, .(Name, elev_mescan = elev_uerra)])



fwrite(dat_all,
       file = "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/martin-01-temp-precip.csv")

fwrite(dat_elev_all,
       file = "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/martin-02-elev.csv")

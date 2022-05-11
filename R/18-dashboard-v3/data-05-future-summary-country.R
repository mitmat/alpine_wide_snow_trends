# conclusion table 
# and some numbers

library(data.table)
library(magrittr)
library(raster)
# library(stars)
library(ggplot2)
library(fs)
library(foreach)

library(flextable)
library(officer)


# elev_int <- 100 
# elev_int <- 200
elev_int <- 500

# breaks

if(elev_int == 200){
  
  l_breaks <- list(bc = c(-10, seq(200, 2600, by = 200), 3000),
                   ds = c(seq(-200, 3600, by = 200), 4200))
  
} else if(elev_int == 500) {
  
  
  l_breaks <- list(bc = c(-10, seq(250, 3000, by = 500), 3000),
                   ds = c(-100, seq(250, 4000, by = 500), 4200))
  
} else stop("check elev_int")


rcms_common <- c("CLMcom-CCLM4-8-17", "CLMcom-ETH-COSMO-crCLIM-v1-1",
                 "CNRM-ALADIN63", "IPSL-WRF381P", "SMHI-RCA4")




# alpine wide: DS ensemble ------------------------------------------------



# common icell 

dat_icell <- readRDS("/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/rds/ds/common-icell.rds")
names(dat_icell)
rcms_common <- c("MODIS", "CLMcom-CCLM4-8-17", "CLMcom-ETH-COSMO-crCLIM-v1-1",
                 "CNRM-ALADIN63", "IPSL-WRF381P", "SMHI-RCA4")
lgl_icell_common <- dat_icell[, 
                              apply(.SD, 1, all), 
                              .SDcols = rcms_common]


# glacier 


rr_rgi <- raster("/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/rgi/modis-hr.tif")
lgl_icell_noglacier <- rr_rgi[] <= 0.1

# countries
rr_modis_countries <- raster("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/modis_1m.tif")
dat_meta_countries <- readRDS("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/meta_1m.rds")

# modis data 

rr_elev <- raster("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/00_aux/eurac_modis_altitude_laea.tif")
rr_elev[rr_elev < -30000] <- NA

rr_water <- raster("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/00_aux/eurac_modis_water_mask_laea.tif")
rr_elev[rr_water == 1] <- NA

rr_modis <- raster("/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/modis_scd/clim/2000-08-01_2020-07-31.tif")


dat_modis <- data.table(icell = 1:ncell(rr_modis),
                        scd = rr_modis[],
                        elev = rr_elev[],
                        country = dat_meta_countries$name_engl[rr_modis_countries[]])
# dat_modis <- dat_modis[!is.na(scd)]
# dat_modis$elev %>% qplot
# dat_modis[lgl_icell_common & elev > 3000, elev] %>% qplot
# dat_modis[lgl_icell_common & lgl_icell_noglacier & elev > 3000, elev] %>% qplot
# dat_modis[lgl_icell_common & lgl_icell_noglacier, elev] %>% summary

dat_modis[, elev_f := cut(elev, breaks = l_breaks$ds, dig.lab = 5)]

dat_modis2 <- dat_modis[lgl_icell_common & lgl_icell_noglacier, 
                        .(scd = mean(scd),
                          nn = .N),
                        .(country, elev_f)]

# dat_modis2 %>% ggplot(aes(scd, elev_f))+geom_point()



## 2041-2070 ---------------------------------------------------------------


# qdm data 

x <- "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/bc_full_clim_2041_2070/03_qdm/"
dat_loop <- data.table(bc_var = path_file(x),
                       file_rcm = dir_ls(x))

dat_loop[, c("institute_rcm", "gcm", "experiment", "ensemble", "dsr", "fp") := 
           tstrsplit(path_ext_remove(path_file(file_rcm)), "_")]

# dat_loop2 <- dat_loop[institute_rcm %in% rcms_common & fp == "future"]
dat_loop2 <- dat_loop[institute_rcm %in% rcms_common]


dat_ds_41 <- foreach(
  i = seq_len(nrow(dat_loop2)),
  .final = rbindlist
) %do% {
  
  # i_rcm <- dat_loop_rcm[i, institute_rcm]
  
  i_rr <- raster(dat_loop2[i, file_rcm])
  
  dat_i <- data.table(dat_modis[, .(icell, elev, elev_f, country)],
                      scd = i_rr[])
  
  dat_i2 <- dat_i[lgl_icell_common & lgl_icell_noglacier,  # 
                  .(scd = mean(scd),
                    nn = .N),
                  .(country, elev_f)]
  
  dat_i3 <- dat_i2 %>% 
    cbind(dat_loop2[i, ])
  dat_i3[, file_rcm := NULL]
  dat_i3
  
  
}




## 2071-2100 ---------------------------------------------------------------


# qdm data 

x <- "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/bc_full_clim/03_qdm/"
dat_loop <- data.table(bc_var = path_file(x),
                       file_rcm = dir_ls(x))

dat_loop[, c("institute_rcm", "gcm", "experiment", "ensemble", "dsr", "fp") := 
           tstrsplit(path_ext_remove(path_file(file_rcm)), "_")]

# dat_loop2 <- dat_loop[institute_rcm %in% rcms_common & fp == "future"]
dat_loop2 <- dat_loop[institute_rcm %in% rcms_common]


dat_ds_71 <- foreach(
  i = seq_len(nrow(dat_loop2)),
  .final = rbindlist
) %do% {
  
  # i_rcm <- dat_loop_rcm[i, institute_rcm]
  
  i_rr <- raster(dat_loop2[i, file_rcm])
  
  dat_i <- data.table(dat_modis[, .(icell, elev, elev_f, country)],
                      scd = i_rr[])
  
  dat_i2 <- dat_i[lgl_icell_common & lgl_icell_noglacier,  # 
                  .(scd = mean(scd),
                    nn = .N),
                  .(country, elev_f)]
  
  dat_i3 <- dat_i2 %>% 
    cbind(dat_loop2[i, ])
  dat_i3[, file_rcm := NULL]
  dat_i3
  
  
}



# alpine wide: BC ensemble ------------------------------------------------


## 2041-2070 ---------------------------------------------------------------


load("/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/rds/bc_full_2041_2070/clim-02.rda")
dat_aux <- readRDS("~/projects-r/clirsnow-bc/data/auxiliary-03-modis.rds")


# add countries
dat_countries_meta <- readRDS("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/meta_1m.rds")
rr_countries <- raster("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/rcm_1m.tif")
dat_countries <- data.table(icell = 1:ncell(rr_countries),
                            val = rr_countries[])
dat_countries[, country := dat_countries_meta$name_engl[val]]
dat_aux <- merge(dat_aux, dat_countries[, .(icell, country)])


# only take pixels in common to all RCMs
dat_th <- readRDS("~/projects-r/clirsnow-bc/data/auxiliary-02-snow-max-threshold.rds")
dat_th[snow_plausible == "yes" & 
         rcm %in% rcms_common] %>% 
  dcast(icell ~ rcm_short, value.var = "snow_plausible") %>% 
  na.omit %>% 
  .[, icell] -> icell_common_rcm
icell_modis <- sort(unique(clim_modis$icell))
icell_common <- intersect(icell_common_rcm, icell_modis)


# elev means
dat_aux[, alt_f := cut(alt, breaks = l_breaks$bc, dig.lab = 5)]

clim_qdm2 <- merge(clim_qdm[icell %in% icell_common], 
                   dat_aux, by = "icell")

dat_bc <- clim_qdm2[,
                    .(snc = mean(snc_qdm),
                      nn = .N),
                    .(country, alt_f, rcm, gcm, experiment, fp)]

dat_bc_41 <- dat_bc[rcm %in% rcms_common]


## 2071-2100 ---------------------------------------------------------------


load("/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/rds/bc_full/clim-02.rda")
dat_aux <- readRDS("~/projects-r/clirsnow-bc/data/auxiliary-03-modis.rds")


# add countries
dat_countries_meta <- readRDS("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/meta_1m.rds")
rr_countries <- raster("/mnt/CEPH_PROJECTS/CLIRSNOW/eurostat/modis-rcm/rcm_1m.tif")
dat_countries <- data.table(icell = 1:ncell(rr_countries),
                            val = rr_countries[])
dat_countries[, country := dat_countries_meta$name_engl[val]]
dat_aux <- merge(dat_aux, dat_countries[, .(icell, country)])


# only take pixels in common to all RCMs
dat_th <- readRDS("~/projects-r/clirsnow-bc/data/auxiliary-02-snow-max-threshold.rds")
dat_th[snow_plausible == "yes" & 
         rcm %in% rcms_common] %>% 
  dcast(icell ~ rcm_short, value.var = "snow_plausible") %>% 
  na.omit %>% 
  .[, icell] -> icell_common_rcm
icell_modis <- sort(unique(clim_modis$icell))
icell_common <- intersect(icell_common_rcm, icell_modis)


# elev means
dat_aux[, alt_f := cut(alt, breaks = l_breaks$bc, dig.lab = 5)]

clim_qdm2 <- merge(clim_qdm[icell %in% icell_common], 
                   dat_aux, by = "icell")

dat_bc <- clim_qdm2[,
                    .(snc = mean(snc_qdm),
                      nn = .N),
                    .(country, alt_f, rcm, gcm, experiment, fp)]

dat_bc_71 <- dat_bc[rcm %in% rcms_common]


# diff --------------------------------------------------------------------

dat_bc_71_diff <- dat_bc_71[fp == "future"] %>%
  merge(dat_bc_71[fp == "past", .(country, alt_f, rcm, gcm, experiment, nn, snc_ref = snc)])


dat_bc_71_diff[, snc_diff := snc - snc_ref]
dat_bc_71_diff[, snc_diff_rel := (snc - snc_ref)/snc_ref]
dat_bc_71_diff[, elev := tstrsplit(alt_f, ",") %>% sapply(readr::parse_number) %>% rowMeans,]


dat_ds_71_diff <- dat_ds_71[fp == "future"] %>%
  merge(dat_ds_71[fp == "past", .(country, elev_f, institute_rcm, gcm, experiment, nn, scd_ref = scd)])


dat_ds_71_diff[, scd_diff := scd - scd_ref]
dat_ds_71_diff[, scd_diff_rel := (scd - scd_ref)/scd_ref]
dat_ds_71_diff[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans,]


# combine & save --------------------------------------------------------------------


dat_bc <- rbindlist(list("2001-2020" = dat_bc_41[fp == "past"],
                         "2041-2070" = dat_bc_41[fp == "future"],
                         "2071-2100" = dat_bc_71[fp == "future"]),
                    idcol = "period")



dat_ds <- rbindlist(list("2001-2020" = dat_ds_41[fp == "past"],
                         "2041-2070" = dat_ds_41[fp == "future"],
                         "2071-2100" = dat_ds_71[fp == "future"]),
                    idcol = "period")

save(dat_bc_71_diff, dat_ds_71_diff, dat_bc, dat_ds,
     file = paste0("data/future-summary-country-elev-", elev_int ,"m.rda"))






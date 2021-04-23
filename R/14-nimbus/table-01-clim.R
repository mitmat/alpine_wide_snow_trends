# clim values of HS, T, P





library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)

library(forcats)
library(foreach)
library(flextable)
library(officer)
library(writexl)



# data --------------------------------------------------------------------

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data4corr-01-merged.rda")


# need to adjust by lapse rate!
dat_elev_eobs <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-02-elev.rds")

dat_hs_apgd_eobs2 <- merge(dat_hs_apgd_eobs, dat_elev_eobs, by = "Name") %>% 
  merge(dat_meta_clust)
dat_hs_apgd_eobs2[, tmean_lapse := tmean + (elev_eobs - Elevation) * 6.5 / 1000]


dat_elev_uerra <-  readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/uerra-02-elev.rds")

dat_hs_uerra_full %>% 
  merge(dat_meta_clust) %>% 
  merge(dat_elev_uerra) -> dat_hs_uerra_full2

dat_hs_uerra_full2[, tmean_lapse := tmean + (elev_uerra - Elevation) * 6.5 / 1000]




# uerra --------------------------------------------------------------

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


# ** 250m -----------------------------------------------------------------




dat_table <- dat_clim[!is.na(HS)] %>% merge(dat_meta_clust, by = "Name")
dat_table[, cluster_fct2 := fct_collapse(cluster_fct,
                                         "Nord" = c("NE", "NW", "North & high Alpine"),
                                         "Sud" = c("SE", "South & high Alpine"))]
dat_table[, elev_fct := cut(Elevation, seq(0, 3000, by = 250), dig.lab = 5)]

dat_table_out <- dat_table[, 
                           .(HS = mean(HS),
                             tmean_lapse = mean(tmean_lapse),
                             prec = mean(prec),
                             nn = .N),
                           .(season, elev_fct, cluster_fct2)]


dat_table_out %>% 
  dcast(season + elev_fct ~ cluster_fct2, 
        value.var = c("HS", "tmean_lapse", "prec", "nn")) -> out_table


write_xlsx(out_table,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/clim-uerra-250m.xlsx")



# ** 500m -----------------------------------------------------------------




dat_table <- dat_clim[!is.na(HS)] %>% merge(dat_meta_clust, by = "Name")
dat_table[, cluster_fct2 := fct_collapse(cluster_fct,
                                         "Nord" = c("NE", "NW", "North & high Alpine"),
                                         "Sud" = c("SE", "South & high Alpine"))]
dat_table[, elev_fct := cut(Elevation, seq(0, 3000, by = 500), dig.lab = 5)]

dat_table_out <- dat_table[, 
                           .(HS = mean(HS),
                             tmean_lapse = mean(tmean_lapse),
                             prec = mean(prec),
                             nn = .N),
                           .(season, elev_fct, cluster_fct2)]



dat_table_out %>% 
  dcast(season + elev_fct ~ cluster_fct2, 
        value.var = c("HS", "tmean_lapse", "prec", "nn")) -> out_table


write_xlsx(out_table,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/clim-uerra-500m.xlsx")

# eobs/apgd ---------------------------------------------------------------


mitmatmisc::add_hydro_year(dat_hs_apgd_eobs2)
mitmatmisc::add_season_fct(dat_hs_apgd_eobs2)

dat_hs_apgd_eobs2[hydro_year %in% c(1981:2010),
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
  .[nn == 28] -> dat_clim


# ** 250m -----------------------------------------------------------------


dat_table <- dat_clim[!is.na(HS)] %>% merge(dat_meta_clust, by = "Name")
dat_table[, cluster_fct2 := fct_collapse(cluster_fct,
                                         "Nord" = c("NE", "NW", "North & high Alpine"),
                                         "Sud" = c("SE", "South & high Alpine"))]
dat_table[, elev_fct := cut(Elevation, seq(0, 3000, by = 250), dig.lab = 5)]

dat_table_out <- dat_table[, 
                           .(HS = mean(HS),
                             tmean_lapse = mean(tmean_lapse),
                             prec = mean(prec),
                             nn = .N),
                           .(season, elev_fct, cluster_fct2)]



dat_table_out %>% 
  dcast(season + elev_fct ~ cluster_fct2, 
        value.var = c("HS", "tmean_lapse", "prec", "nn")) -> out_table


write_xlsx(out_table,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/clim-eobs-apgd-250m.xlsx")



# ** 500m -----------------------------------------------------------------




dat_table <- dat_clim[!is.na(HS)] %>% merge(dat_meta_clust, by = "Name")
dat_table[, cluster_fct2 := fct_collapse(cluster_fct,
                                         "Nord" = c("NE", "NW", "North & high Alpine"),
                                         "Sud" = c("SE", "South & high Alpine"))]
dat_table[, elev_fct := cut(Elevation, seq(0, 3000, by = 500), dig.lab = 5)]

dat_table_out <- dat_table[, 
                           .(HS = mean(HS),
                             tmean_lapse = mean(tmean_lapse),
                             prec = mean(prec),
                             nn = .N),
                           .(season, elev_fct, cluster_fct2)]


dat_table_out %>% 
  dcast(season + elev_fct ~ cluster_fct2, 
        value.var = c("HS", "tmean_lapse", "prec", "nn")) -> out_table


write_xlsx(out_table,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/clim-eobs-apgd-500m.xlsx")



library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
# library(patchwork)

library(forcats)
library(foreach)
library(flextable)
library(officer)




# data --------------------------------------------------------------------

dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data4corr-01-merged.rda")


# need to adjust by lapse rate!
dat_elev_eobs <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-02-elev.rds")

dat_hs_apgd_eobs2 <- merge(dat_hs_apgd_eobs, dat_elev_eobs, by = "Name") %>% 
  merge(dat_meta_clust)
dat_hs_apgd_eobs2[, tmean_lapse := tmean + (elev_eobs - Elevation) * 6.5 / 1000]


dat_elev_uerra <-  readRDS("~/alps-snow/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/uerra-02-elev.rds")

dat_hs_uerra_full %>% 
  merge(dat_meta_clust) %>% 
  merge(dat_elev_uerra) -> dat_hs_uerra_full2

dat_hs_uerra_full2[, tmean_lapse := tmean + (elev_uerra - Elevation) * 6.5 / 1000]




# djf + mam hs t p --------------------------------------------------------------

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



dat_hs_apgd_eobs2[hydro_year %in% c(1981:2010) & season == "DJF",
                   .(HS = mean(HS),
                     tmean_lapse = mean(tmean_lapse),
                     prec = sum(prec),
                     nn = .N),
                   .(Name, hydro_year, season)] %>% 
  .[nn == 3] %>% 
  .[, 
    .(corr = c(cor(HS, tmean_lapse), cor(HS, prec)),
      climval = c("tmean", "prec"),
      nn = .N),
    .(Name, season)] %>% 
  .[nn == 28] -> dat_cor_djf



dat_hs_apgd_eobs2[hydro_year %in% c(1981:2010),
                   .(HS = mean(HS),
                     tmean_lapse = mean(tmean_lapse),
                     prec = sum(prec),
                     nn = .N),
                   .(Name, hydro_year)] %>% 
  .[nn == 7] %>% 
  .[, 
    .(corr = c(cor(HS, tmean_lapse), cor(HS, prec)),
      climval = c("tmean", "prec"),
      nn = .N),
    .(Name)] %>% 
  .[nn == 28] -> dat_cor_ndjfmam

# plot clim --------------------------------------------------------------------

dat_plot <- merge(dat_clim[!is.na(HS)], dat_meta_clust, by = "Name")


gg1_hs <- dat_plot %>% 
  ggplot(aes(HS, Elevation, color = cluster_fct))+
  geom_point(size = 0.5)+
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_grid(. ~ season, scales = "free")+
  ylab("Elevation [m]")+
  xlab("Mean snow depth [cm]")


ggsave(filename = "fig/hs-clim.png",
       gg1_hs,
       width = 10, height = 3.5)



gg1_hs_noregion <-
dat_plot %>% 
  ggplot(aes(HS, Elevation))+
  geom_point(size = 0.5)+
  geom_smooth(se = F, method=loess)+
  # scale_color_brewer("Region", palette = "Set1")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  facet_grid(. ~ season, scales = "free")+
  ylab("Elevation [m]")+
  xlab("Mean snow depth [cm]")


ggsave(filename = "fig/hs-clim-noregion.png",
       gg1_hs_noregion,
       width = 8, height = 4)


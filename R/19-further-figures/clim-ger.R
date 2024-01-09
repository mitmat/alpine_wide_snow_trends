
library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
library(patchwork)
# library(emojifont)
# library(ggimage)

library(forcats)
# library(foreach)
# library(flextable)
# library(officer)

# library(mgcv)


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
dat_plot[, cluster_fct2 := fct_collapse(cluster_fct, 
  "Nord" = c("NE", "NW", "North & high Alpine"),
  "Süd" = c("SE", "South & high Alpine")
)]
dat_plot[, cluster_fct_reorder := fct_relevel(cluster_fct,
                                              "NW", "NE", "SE", 
                                              "North & high Alpine",
                                              "South & high Alpine")]

# 
# gg1_hs <- dat_plot %>% 
#   ggplot(aes(HS, Elevation, color = cluster_fct))+
#   geom_point(size = 0.5)+
#   scale_color_brewer("Region", palette = "Set1")+
#   theme_bw()+
#   theme(panel.grid.minor = element_blank())+
#   facet_grid(. ~ season, scales = "free")+
#   ylab("Elevation [m]")+
#   xlab("Mean snow depth [cm]")
# 
# 
# ggsave(filename = "fig/hs-clim-DJF.png",
#        gg1_hs,
#        width = 10, height = 3.5)


gg_icon_snow <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("snowflake", x = 200, y = 700, size = 20)

gg_icon_tmean <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("thermometer", x = 200, y = 700, size = 20)

gg_icon_prec <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("cloud_with_rain", x = 200, y = 700, size = 20)



gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(HS, Elevation))+
  geom_point(size = 0.5)+
  geom_smooth(se = F, method=loess)+
  # geom_emoji("snowflake", x = 200, y = 700, size = 50, color = "#2b8cbe")+
  # scale_color_brewer("Region", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank())+
  # facet_grid(. ~ season, scales = "free")+
  ylab("Höhe [m]")+
  xlab("Mittlere Schneehöhe Winter (Dez-Feb) [cm]")

gg_out <- gg1+inset_element(gg_icon_snow, 0.7, 0.0, 1, 0.4)

ggsave(filename = "fig/clim-hs-DJF.png",
       gg_out,
       width = 6, height = 4)



gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(tmean_lapse, Elevation))+
  geom_point(size = 0.5)+
  # geom_emoji("thermometer", x =-10, y = 700, size = 50)+
  # geom_smooth(se = F, method=loess)+
  # scale_color_brewer("Region", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank())+
  # facet_grid(. ~ season, scales = "free")+
  ylab("Höhe [m]")+
  xlab("Mittlere Temperatur Winter (Dez-Feb) [°C]")

gg_out <- gg1+inset_element(gg_icon_tmean, 0.0, 0.0, 0.3, 0.4)


ggsave(filename = "fig/clim-tmean-DJF.png",
       gg_out,
       width = 6, height = 4)


gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(prec, Elevation))+
  geom_point(size = 0.5)+
  # geom_emoji("cloud_with_rain", x = 500, y = 2300, size = 50, color = "#bdd7e7")+
  # geom_smooth(se = F, method=loess)+
  # scale_color_brewer("Region", palette = "Set1")+
  theme_bw(18)+
  theme(panel.grid.minor = element_blank())+
  # facet_grid(. ~ season, scales = "free")+
  ylab("Höhe [m]")+
  xlab("Mittlerer Niederschlag Winter (Dez-Feb) [mm]")

gg_out <- gg1+inset_element(gg_icon_prec, 0.7, 0.6, 1.0, 1.0)



ggsave(filename = "fig/clim-prec-DJF.png",
       gg_out,
       width = 6, height = 4)


# 
# dat_plot[season == "DJF"] %>% 
#   ggplot(aes(prec, Elevation))+
#   geom_point(data = dat_plot[season == "DJF", .(prec, Elevation)], 
#              size = 0.5, colour = "grey")+
#   geom_point(aes(colour = cluster_fct2),
#              size = 0.5)+
#   # geom_smooth(se = F, method=loess)+
#   scale_color_brewer("Region", palette = "Dark2")+
#   theme_bw(14)+
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none")+
#   facet_wrap(~ cluster_fct2)+
#   ylab("Höhe [m]")+
#   xlab("Mittlerer Niederschlag Winter (Dez-Feb) [mm]")
# 
# 
# ggsave(filename = "fig/clim-prec-DJF-region2.png",
#        # gg1_hs,
#        width = 8, height = 4)

gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(prec, Elevation))+
  geom_point(data = dat_plot[season == "DJF", .(prec, Elevation)], 
             size = 0.5, colour = "grey")+
  geom_point(aes(colour = cluster_fct),
             size = 0.5)+
  # geom_smooth(se = F, method=loess)+
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~ cluster_fct_reorder)+
  ylab("Höhe [m]")+
  xlab("Mittlerer Niederschlag Winter (Dez-Feb) [mm]")

gg_out <- gg1+inset_element(gg_icon_prec, 0.7, 0.0, 1.0, 0.4)



ggsave(filename = "fig/clim-prec-DJF-region5.png",
       gg_out,
       width = 8, height = 5)


gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(HS, Elevation))+
  geom_point(data = dat_plot[season == "DJF", .(HS, Elevation)], 
             size = 0.5, colour = "grey")+
  geom_point(aes(colour = cluster_fct),
             size = 0.5)+
  # geom_smooth(se = F, method=loess)+
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~ cluster_fct_reorder)+
  ylab("Höhe [m]")+
  xlab("Mittlere Schneehöhe Winter (Dez-Feb) [cm]")

gg_out <- gg1+inset_element(gg_icon_snow, 0.7, 0.0, 1.0, 0.4)



ggsave(filename = "fig/clim-HS-DJF-region5.png",
       gg_out,
       width = 8, height = 5)



gg1 <- dat_plot[season == "DJF"] %>% 
  ggplot(aes(tmean_lapse, Elevation))+
  geom_point(data = dat_plot[season == "DJF", .(tmean_lapse, Elevation)], 
             size = 0.5, colour = "grey")+
  geom_point(aes(colour = cluster_fct),
             size = 0.5)+
  # geom_smooth(se = F, method=loess)+
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+
  facet_wrap(~ cluster_fct_reorder)+
  ylab("Höhe [m]")+
  xlab("Mittlere Temperatur Winter (Dez-Feb) [°C]")

gg_out <- gg1+inset_element(gg_icon_tmean, 0.7, 0.0, 1.0, 0.4)

ggsave(filename = "fig/clim-tmean-DJF-region5.png",
       gg_out,
       width = 8, height = 5)



# dat_plot[season == "MAM"] %>% 
#   ggplot(aes(tmean_lapse, Elevation))+
#   geom_point(data = dat_plot[season == "MAM", .(tmean_lapse, Elevation)], 
#              size = 0.5, colour = "grey")+
#   geom_point(aes(colour = cluster_fct),
#              size = 0.5)+
#   # geom_smooth(se = F, method=loess)+
#   scale_color_brewer("Region", palette = "Set1")+
#   theme_bw(14)+
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "none")+
#   facet_wrap(~ cluster_fct_reorder)+
#   ylab("Höhe [m]")+
#   xlab("Mittlere Temperatur Frühling (Mär-Mai) [°C]")
# 
# 
# ggsave(filename = "fig/clim-tmean-MAM-region5.png",
#        # gg1_hs,
#        width = 8, height = 5)




# zones -------------------------------------------------------------------

library(sf)

sf_meta <- st_as_sf(dat_meta_clust,
                        coords = c("Longitude", "Latitude"),
                        crs = 4326)
sf_meta <- sf_meta %>% dplyr::mutate(Region = cluster_fct)
# sf_meta <- sf_meta %>% mutate(lbl = paste0(Name, " (", Elevation, "m)"))


sf_borders <- st_as_sf(maps::map("world", plot = F, fill = T)) %>% 
  dplyr::filter(ID %in% c("Austria", "France", "Germany",
                          "Italy", "Slovenia", "Switzerland",
                          "Croatia", "Hungary", "Czech Republic",
                          "Slovakia"))

sf_borders2 <- sf_borders %>% 
  dplyr::mutate(geom = st_cast(geom, "MULTILINESTRING")) %>% 
  # st_crop(sf_meta)
  st_crop(c(xmin = 4.5, ymin = 42.9, xmax = 18, ymax = 49.5))

# gg <- 
ggplot()+
  # borders()+
  geom_sf(data = sf_borders2)+
  geom_sf(data = sf_meta, aes(colour = Region, shape = Region))+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_sf(xlim = range(dat_meta_clust$Longitude), ylim = range(dat_meta_clust$Latitude))+
  theme_bw()+
  theme(legend.position = c(0.65, 0.15),
    # legend.position = "top",
    legend.title = element_blank(),
    legend.background = element_rect(colour = "grey"),
    legend.direction = "horizontal",
    legend.box.just = "right")+
  scale_shape("")+
  scale_color_brewer("", palette = "Set1")


ggsave(filename = "fig/zones-simple.png",
       width = 8, height = 5)




# deviation from elev -----------------------------------------------------

# not working so well...

dat_plot[season == "DJF"] %>% 
  ggplot(aes(Elevation, tmean_lapse))+
  geom_point()+
  geom_smooth(method = "gam")


dat_plot_elev_anom <- dat_plot[season == "DJF"]
dat_plot_elev_anom[, HS_resid := resid(gam(HS ~ s(Elevation)))]  
dat_plot_elev_anom[, tmean_resid := resid(gam(tmean_lapse ~ s(Elevation)))]  

dat_plot_elev_anom %>% 
ggplot(aes(Elevation, HS_resid/HS, colour = cluster_fct2))+
  geom_point()+
  facet_wrap(~cluster_fct2)+
  # ylim(-1,1)
  xlim(1000, NA)


dat_plot_elev_anom %>% 
  ggplot(aes(Elevation, HS_resid, colour = cluster_fct2))+
  geom_point()+
  facet_wrap(~cluster_fct)


dat_plot_elev_anom %>% 
  ggplot(aes(Elevation, tmean_resid, colour = cluster_fct))+
  geom_point()+
  facet_wrap(~cluster_fct2)

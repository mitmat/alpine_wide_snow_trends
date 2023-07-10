# plot trends (full long period)


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)

library(forcats)
library(foreach)
library(flextable)
library(officer)

pval_sym <- function(pval){
  symnum(pval, corr = FALSE,
         cutpoints = c(0,  .001,.01,.05, .1, 1),
         symbols = c("***","**","*","."," "))
}

dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")


# check if ols or gls -----------------------------------------------------


dat_month_gls[term == "year0"] %>% summary
dat_month_gls[term == "year0", table(pval_sym(p.value.var))]
dat_month_gls[term == "year0", table(pval_sym(p.value.var), month)]
dat_month_gls[term == "year0", table(p.value.var < 0.1, month)] %>% prop.table(2)

dat_seasonal_gls[term == "year0", table(pval_sym(p.value.var))]
dat_seasonal_gls[term == "year0", table(pval_sym(p.value.var), variable)]
dat_seasonal_gls[term == "year0", table(p.value.var < 0.1, variable)] %>% prop.table(2)



# mean and max HS (absolute) ------------------------------------------------------


dat_plot_full <- dat_seasonal_gls[term == "year0" & !startsWith(variable, "SCD")] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_full[, est_low := estimate - 1.96 * std.error]
dat_plot_full[, est_high := estimate + 1.96 * std.error]
dat_plot_full[, variable_fct := fct_relevel(factor(variable), "maxHS_NDJFMAM", after = Inf)]
dat_plot_full


# manual limits y (elev)
dat_ylim <- dat_plot_full[, .(max_elev = max(Elevation)), .(cluster_fct)] 
setorder(dat_ylim, cluster_fct)
dat_ylim[, max_elev := c(1250, 1250, 3000, 3000, 1250)]


gg <-
  dat_plot_full[variable %in% c("meanHS_DJF", "meanHS_MAM")] %>% 
  ggplot(aes(estimate*10, Elevation, xmin = est_low*10, xmax = est_high*10))+
  geom_vline(xintercept = 0)+
  geom_pointrange(size = 0.1, fatten = 0.5)+
  # geom_jitter(width = 0, height = 0.3)+
  # facet_wrap(~month_fct, nrow = 2)+
  facet_grid(. ~ variable_fct, scales = "free", space = "free")+ # free_x or free
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  # scale_color_brewer("", palette = "Set1", guide = F)+
  # scale_x_continuous(breaks = 10*seq(-4, 4, by = 2))+
  # scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, NA))+
  # geom_blank(inherit.aes = F, data = dat_ylim, aes(x = 0, y = max_elev))+
  xlab("Linear trend in seasonal HS indices [cm per decade]")+
  ylab("Elevation [m]")


ggsave(gg,
       filename = "fig/hs-seasonal-trends-noregion.png",
       width = 10,
       height = 5)


# collapsed two zones -----------------------------------------------------



dat_plot_full[, cluster_fct2 := fct_collapse(
  cluster_fct,
  North = c("NW", "NE", "North & high Alpine"),
  South = c("South & high Alpine", "SE")
)]

gg <-
  # dat_plot_full[variable %in% c("meanHS_DJF", "meanHS_MAM")] %>% 
  dat_plot_full[variable %in% c("meanHS_DJF")] %>% 
  ggplot(aes(estimate*10, Elevation, xmin = est_low*10, xmax = est_high*10, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  geom_linerange(alpha = 0.3)+
  facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw(16)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+ # 
  xlab("Linear trend in meanHS_DJF [cm per decade]")+
  ylab("Elevation [m]")


ggsave(gg,
       filename = "fig/hs-seasonal-trends-DJF.png",
       width = 12,
       height = 6)


gg <-
  # dat_plot_full[variable %in% c("meanHS_DJF", "meanHS_MAM")] %>% 
  dat_plot_full[variable %in% c("meanHS_MAM")] %>% 
  ggplot(aes(estimate*10, Elevation, xmin = est_low*10, xmax = est_high*10, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  geom_linerange(alpha = 0.3)+
  facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_color_brewer("Region", palette = "Set1")+
  theme_bw(16)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+ # 
  xlab("Linear trend in meanHS_MAM [cm per decade]")+
  ylab("Elevation [m]")


ggsave(gg,
       filename = "fig/hs-seasonal-trends-MAM.png",
       width = 12,
       height = 6)

# mean and max HS (relative) ------------------------------------------------------

dat_plot_full2 <- dat_seasonal_gls[term == "year0" & !startsWith(variable, "SCD")] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_full2[, est_low := trend.rel - 1.96 * trend.rel.se]
dat_plot_full2[, est_high := trend.rel + 1.96 * trend.rel.se]
dat_plot_full2[, variable_fct := fct_relevel(factor(variable), "maxHS_NDJFMAM", after = Inf)]
dat_plot_full2


# manual limits y (elev)
dat_ylim <- dat_plot_full2[, .(max_elev = max(Elevation)), .(cluster_fct)] 
setorder(dat_ylim, cluster_fct)
dat_ylim[, max_elev := c(1250, 1250, 3000, 3000, 1250)]

# remove high relative
dat_plot_full2[abs(trend.rel*10) > 1]
dat_plot_full2_sub <- dat_plot_full2[abs(trend.rel*10) < 1]
dat_plot_full2_sub <- dat_plot_full2


dat_plot_full2_sub[, cluster_fct2 := fct_collapse(
  cluster_fct,
  North = c("NW", "NE", "North & high Alpine"),
  South = c("South & high Alpine", "SE")
)]

gg <-
  dat_plot_full2_sub[variable %in% c("meanHS_DJF")] %>% 
  ggplot(aes(trend.rel*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 1)+
  facet_grid(. ~ cluster_fct2)+ # free_x or free
  theme_bw(16)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  scale_color_brewer("", palette = "Set1")+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, NA))+
  xlab("Linear trend in meanHS_DJF [% per decade]")+
  ylab("Elevation [m]")


ggsave(gg,
       filename = "fig/hs-seasonal-trends-DJF-rel.png",
       width = 12,
       height = 6)



gg <-
  dat_plot_full2_sub[variable %in% c("meanHS_MAM")] %>% 
  ggplot(aes(trend.rel*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 1)+
  facet_grid(. ~ cluster_fct2)+ # free_x or free
  theme_bw(16)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  scale_color_brewer("", palette = "Set1")+
  scale_x_continuous(labels = scales::percent_format(),
                     limits = c(NA, .2), oob = scales::oob_squish)+
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, NA))+
  xlab("Linear trend in meanHS_MAM [% per decade]")+
  ylab("Elevation [m]")


ggsave(gg,
       filename = "fig/hs-seasonal-trends-MAM-rel.png",
       width = 12,
       height = 6)




# by meanHS instead of elev -----------------------------------------------

load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data4corr-01-merged.rda")
# need to adjust by lapse rate!
dat_elev_eobs <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/08_AUX_GRIDS/extract-stn/eobs-02-elev.rds")

dat_hs_apgd_eobs2 <- merge(dat_hs_apgd_eobs, dat_elev_eobs, by = "Name") %>% 
  merge(dat_meta_clust)
dat_hs_apgd_eobs2[, tmean_lapse := tmean + (elev_eobs - Elevation) * 6.5 / 1000]


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




dat_plot_full2_sub[variable %in% c("meanHS_DJF")] %>% 
  merge(dat_clim[season == "DJF"]) %>% 
  ggplot(aes(trend.rel*10, HS, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 1)+
  facet_grid(. ~ cluster_fct2)+ # free_x or free
  theme_bw(16)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom")+
  scale_color_brewer("", palette = "Set1")+
  scale_x_continuous(labels = scales::percent_format())+
  # scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, NA))+
  xlab("Linear trend in meanHS_DJF [% per decade]")+
  ylab("Elevation [m]")

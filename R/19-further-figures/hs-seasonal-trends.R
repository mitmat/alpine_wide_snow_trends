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


# gg <-
  dat_plot_full2_sub[variable %in% c("meanHS_DJF", "meanHS_MAM")] %>% 
  ggplot(aes(trend.rel*10, Elevation))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.1)+
  # geom_pointrange(size = 0.1, fatten = 0.5)+
  # geom_jitter(width = 0, height = 0.3)+
  # facet_wrap(~month_fct, nrow = 2)+
  facet_grid(. ~ variable_fct, scales = "free", space = "free")+ # free_x or free
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  # scale_color_brewer("", palette = "Set1", guide = F)+
  # scale_x_continuous(breaks = 10*seq(-4, 4, by = 2))+
    scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(breaks = seq(0, 3000, by = 500), limits = c(0, NA))+
  # geom_blank(inherit.aes = F, data = dat_ylim, aes(x = 0, y = max_elev))+
  xlab("Linear trend in seasonal HS indices [% per decade]")+
  ylab("Elevation [m]")


# ggsave(gg,
#        filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/fig/zenodo-SM/trends-seasonalHS-GLS-relative.png",
#        width = 10,
#        height = 11)




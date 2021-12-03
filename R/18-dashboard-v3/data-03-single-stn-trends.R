

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
# library(patchwork)
# library(flextable)
# library(officer)


dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")

dat_plot_month <- dat_month_gls[term == "year0"] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_month[, est_low := estimate - 1.96 * std.error]
dat_plot_month[, est_high := estimate + 1.96 * std.error]
mitmatmisc::add_month_fct(dat_plot_month, 10)
dat_plot_month[, hilo := fct_collapse(cluster_fct, 
                                      high = c("South & high Alpine", "North & high Alpine"),
                                      low = c("NW", "NE", "SE"))]
dat_plot_month[, Trend := round(estimate*10, 2)]
dat_plot_month[, Region := cluster_fct] 




dat_plot_season_hs <- dat_seasonal_gls[term == "year0" & !startsWith(variable, "SCD")] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_season_hs[, Trend := round(estimate*10, 2)]
dat_plot_season_hs[, Region := cluster_fct]


dat_plot_season_scd <- dat_seasonal_ols[term == "year0" & startsWith(variable, "SCD")] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_season_scd[, Trend := round(estimate*10, 2)]
dat_plot_season_scd[, Region := cluster_fct]


save(dat_plot_month, dat_plot_season_scd, dat_plot_season_hs,
     file = "data/single-stn-trends.Rdata")




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
dat_month <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")

load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")



# 1000 m ------------------------------------------------------------------
elev_int <- 1000


## table absolute ----------------------------------------------------------

dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate)] %>% 
  merge(dat_meta_clust, by = "Name")


dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]

dat_trends_abs <- dat_table[,
                            .(nn = .N,
                              mean = 49*mean(estimate), 
                              min = 49*min(estimate), 
                              max = 49*max(estimate),
                              sd = 49*sd(estimate)),
                            .(elev_fct, ns_fct, variable)]



## table relative ----------------------------------------------------------


dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate = 100 * trend.rel)] %>%
  merge(dat_meta_clust, by = "Name")

dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]

# remove MAM low elevation with too high relative numbers (and insignificant snow)
dat_table[variable == "meanHS_MAM" & estimate > 5] 
dat_table[50*estimate > 100] 


dat_trends_rel <- dat_table[!(Name %in% c("Gratkorn", "Plave", "Sajach", "Lendava", "Colnica_lig")),
                            .(nn = .N,
                              mean = 49*mean(estimate), 
                              min = 49*min(estimate),
                              max = 49*max(estimate),
                              sd = 49*sd(estimate)),
                            .(elev_fct, ns_fct, variable)]



## save --------------------------------------------------------------------


dat_trends_abs
dat_trends_rel

save(dat_trends_abs, dat_trends_rel, file = "data/trends_1000m.Rdata")


# 500 m ------------------------------------------------------------------

elev_int <- 500

## table absolute ----------------------------------------------------------

dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate)] %>% 
  merge(dat_meta_clust, by = "Name")


dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]

dat_trends_abs <- dat_table[,
                            .(nn = .N,
                              mean = 49*mean(estimate), 
                              min = 49*min(estimate), 
                              max = 49*max(estimate),
                              sd = 49*sd(estimate)),
                            .(elev_fct, ns_fct, variable)]



## table relative ----------------------------------------------------------


dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate = 100 * trend.rel)] %>%
  merge(dat_meta_clust, by = "Name")

dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]

# remove MAM low elevation with too high relative numbers (and insignificant snow)
dat_table[variable == "meanHS_MAM" & estimate > 5] 
dat_table[50*estimate > 100] 


dat_trends_rel <- dat_table[!(Name %in% c("Gratkorn", "Plave", "Sajach", "Lendava", "Colnica_lig")),
                            .(nn = .N,
                              mean = 49*mean(estimate), 
                              min = 49*min(estimate),
                              max = 49*max(estimate),
                              sd = 49*sd(estimate)),
                            .(elev_fct, ns_fct, variable)]



## save --------------------------------------------------------------------


dat_trends_abs
dat_trends_rel

save(dat_trends_abs, dat_trends_rel, file = "data/trends_500m.Rdata")




# trends avg first --------------------------------------------------------

# slightly different
# 
# elev_int <- 1000
# dat_seasonal %>% 
#   merge(dat_meta_clust, by = "Name") -> dat_plot_ts
# dat_plot_ts[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]
# dat_plot_ts[, ns_fct := fct_collapse(cluster_fct,
#                                      "North" = c("NE", "NW", "North & high Alpine"),
#                                      "South" = c("South & high Alpine", "SE"))]
# 
# 
# dat_plot_ts[, value_stn_mean := mean(value), .(Name, variable)]
# dat_plot_ts[, value_anomaly := value - value_stn_mean]
# 
# 
# dat_plot_ts_mean <- dat_plot_ts[, 
#                                 .(mean_value = mean(value),
#                                   mean_value_anomaly = mean(value_anomaly),
#                                   nn = .N),
#                                 .(year, variable, elev_fct, ns_fct)]
# 
# 
# dat_plot_ts_mean[, year0 := year - min(year)]
# # dat_plot_ts_mean[nn > 5,
# dat_plot_ts_mean[,
#                  broom::tidy(lm(mean_value ~ year0)),
#                  .(variable, elev_fct, ns_fct, nn)] %>% 
#   dcast(variable + elev_fct + ns_fct + nn ~ term, value.var = "estimate") -> dat_table_start_end
# setnames(dat_table_start_end, c("variable", "elevation", "region", "nn" ,"year1971", "trend"))
# 
# dat_table_start_end[, year2019 := year1971 + 49*trend]
# dat_table_start_end[, trend_abs := round(49*trend, 1)]
# dat_table_start_end[, trend_rel := round(49*trend / year1971, 3)]


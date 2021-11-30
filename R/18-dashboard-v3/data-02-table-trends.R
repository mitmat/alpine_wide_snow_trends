

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



# table absolute ----------------------------------------------------------

dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate)] %>% 
  merge(dat_meta_clust, by = "Name")


dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, 0:3*1000, dig.lab = 6)]

dat_trends_abs <- dat_table[,
                            .(nn = .N,
                              mean = 49*mean(estimate), 
                              min = 49*min(estimate), 
                              max = 49*max(estimate),
                              sd = 49*sd(estimate)),
                            .(elev_fct, ns_fct, variable)]



# table relative ----------------------------------------------------------


dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate = 100 * trend.rel)] %>%
  merge(dat_meta_clust, by = "Name")

dat_table[, ns_fct := fct_collapse(cluster_fct,
                                   "North" = c("NE", "NW", "North & high Alpine"),
                                   "South" = c("South & high Alpine", "SE"))]
dat_table[, elev_fct := cut(Elevation, 0:3*1000, dig.lab = 6)]

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



# save --------------------------------------------------------------------


dat_trends_abs
dat_trends_rel

save(dat_trends_abs, dat_trends_rel, file = "data/trends.Rdata")


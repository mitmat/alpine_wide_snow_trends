

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
library(patchwork)
library(flextable)
library(officer)

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")

load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")


elev_int <- 1000
# elev_int <- 500

# table: trends -------------------------------------------------------------------



dat_table <- dat_seasonal_gls[term == "year0",
                              .(Name, variable, estimate_abs = estimate, estimate_rel = 100 * trend.rel)] %>% 
  merge(dat_meta_clust, by = "Name")

dat_table[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]

dat_conclusion_table <- dat_table[!(Name %in% c("Gratkorn", "Plave", "Sajach", "Lendava", "Colnica_lig")),
                                  .(nn = .N,
                                    mean_abs = 49*mean(estimate_abs),
                                    mean_rel = 49*mean(estimate_rel)),
                                  keyby = .(variable, elev_fct)]


writexl::write_xlsx(dat_conclusion_table,
                    paste0("/mnt/CEPH_PROJECTS/CLIRSNOW/00_exchange/table_matteo_trend_",
                    elev_int, "m.xlsx"))



# table start and end -----------------------------------------------------

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")




dat_seasonal %>% 
  merge(dat_meta_clust, by = "Name") -> dat_plot_ts
dat_plot_ts[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]
dat_plot_ts[, value_stn_mean := mean(value), .(Name, variable)]
dat_plot_ts[, value_anomaly := value - value_stn_mean]

dat_plot_ts_mean <- dat_plot_ts[, 
                                .(mean_value = mean(value),
                                  mean_value_anomaly = mean(value_anomaly),
                                  nn = .N),
                                .(year, variable, elev_fct)]


dat_plot_ts_mean[, year0 := year - min(year)]
dat_plot_ts_mean[nn > 5,
                 tidy(lm(mean_value ~ year0)),
                 .(variable, elev_fct, nn)] %>% 
  dcast(variable + elev_fct + nn ~ term, value.var = "estimate") -> dat_table_start_end
setnames(dat_table_start_end, c("variable", "elevation", "nn" ,"year1971", "trend"))

dat_table_start_end[, year2019 := year1971 + 49*trend]
dat_table_start_end[, trend := NULL]

writexl::write_xlsx(dat_table_start_end,
                    paste0("/mnt/CEPH_PROJECTS/CLIRSNOW/00_exchange/table_matteo_start_end_",
                    elev_int, "m.xlsx"))


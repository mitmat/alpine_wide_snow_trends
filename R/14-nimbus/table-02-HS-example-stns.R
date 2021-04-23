# some station values

# ~50 stn
# values: monthly HS, maxHS, meanSCD

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



# prep data ---------------------------------------------------------------

dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/rds/meta-with-cluster-01.rds")

dat_meta[, cluster_fct2 := fct_collapse(cluster_fct,
                                         "Nord" = c("NE", "NW", "North & high Alpine"),
                                         "Sud" = c("SE", "South & high Alpine"))]

dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/rds/data-04-monthly-1981-2010.rds")
dat_season <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/rds/data-05-seasonal-1981-2010.rds")

dat_month <- mitmatmisc::add_month_fct(dat_month, 10)
dat_month[, .(meanHS = mean(HS)), .(Name, month_fct)] %>% 
  dcast(Name ~ month_fct, value.var = "meanHS") -> dat_clim_month
setnames(dat_clim_month, 2:8, paste0("meanHS_", names(dat_clim_month)[2:8]))

dat_season[variable %in% c("maxHS_NDJFMAM", "SCD_NDJFMAM"),
           .(mean_value = mean(value)),
           .(Name, variable)] %>% 
  dcast(Name ~ variable, value.var = "mean_value") -> dat_clim_season

dat_meta[, .(Name, Region = cluster_fct2, 
             Country = substr(Provider, 1, 2), Elevation, Longitude, Latitude)] %>% 
  merge(dat_clim_month) %>% 
  merge(dat_clim_season) -> dat_all


write_xlsx(dat_all,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/snow-stns-01-all.xlsx")

# subset to some stations -------------------------------------------------

# avg per
# - N&S
# - 500m elev
# - country


dat_all[, .(Name, Country, val = maxHS_NDJFMAM)] %>% 
  merge(dat_meta) -> dat_check
dat_check[, elev_fct := cut(Elevation, seq(0, 3000, by = 500), dig.lab = 5)]

dat_check[, mean_val := mean(val), .(cluster_fct2, elev_fct, Country)]

# avg
dat_avg <- dat_check[, .SD[which.min(abs(val - mean_val))], .(cluster_fct2, elev_fct, Country)]

out1 <- dat_all[Name %in% dat_avg$Name]



write_xlsx(out1,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/snow-stns-02-avg_NS-country-500m.xlsx")



# subset to some stations 2 -------------------------------------------------

# avg and most extreme per
# - N&S
# - 500m elev


dat_all[, .(Name, Country, val = maxHS_NDJFMAM)] %>% 
  merge(dat_meta) -> dat_check
dat_check[, elev_fct := cut(Elevation, seq(0, 3000, by = 500), dig.lab = 5)]

dat_check[, mean_val := mean(val), .(cluster_fct2, elev_fct)]


dat_avg <- dat_check[, .SD[which.min(abs(val - mean_val))], .(cluster_fct2, elev_fct)]

dat_min <- dat_check[, .SD[which.min(val - mean_val)], .(cluster_fct2, elev_fct)]
dat_max <- dat_check[, .SD[which.max(val - mean_val)], .(cluster_fct2, elev_fct)]


out2 <- dat_all[Name %in% c(dat_avg$Name, dat_min$Name, dat_max$Name)]



write_xlsx(out2,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/snow-stns-03-avg-min-max_NS-500m.xlsx")




# subset to some stations 3 -------------------------------------------------

# avg and most extreme per
# - country
# - 500m elev


dat_all[, .(Name, Country, val = maxHS_NDJFMAM)] %>% 
  merge(dat_meta) -> dat_check
dat_check[, elev_fct := cut(Elevation, seq(0, 3000, by = 500), dig.lab = 5)]

dat_check[, mean_val := mean(val), .(Country, elev_fct)]


dat_avg <- dat_check[, .SD[which.min(abs(val - mean_val))], .(Country, elev_fct)]

dat_min <- dat_check[, .SD[which.min(val - mean_val)], .(Country, elev_fct)]
dat_max <- dat_check[, .SD[which.max(val - mean_val)], .(Country, elev_fct)]


out3 <- dat_all[Name %in% c(dat_avg$Name, dat_min$Name, dat_max$Name)]



write_xlsx(out3,
           "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/table/snow-stns-04-avg-min-max_country-500m.xlsx")



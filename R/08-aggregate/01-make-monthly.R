# make monthly data

library(data.table)
library(magrittr)
library(lubridate)


min_frac_avail <- 0.9




# from QC -----------------------------------------------------------------


# pre 1960
dat_long <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1787hn_1879hs-1960/data_long_HN_HS.rds")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds")

dat_month <- dat_long[, .(HN = sum(HN, na.rm = T),
                          HS = mean(HS, na.rm = T),
                          nn_HN = sum(!is.na(HN)),
                          nn_HS = sum(!is.na(HS)),
                          nn_in_month = days_in_month(Date[1])),
                      .(Name, year(Date), month(Date))]

dat_month[nn_HN < min_frac_avail * nn_in_month, HN := NA]
dat_month[nn_HS < min_frac_avail * nn_in_month, HS := NA]

out_data <- dat_month[!is.na(HS) | !is.na(HN), .(Name, year, month, HN, HS)]
out_meta <- dat_meta[Name %in% unique(out_data$Name)]

out_data[, .(year = min(year) : max(year)), .(Name)] %>% 
  .[, .(month = 1:12), .(Name, year)] %>% 
  merge(out_data, all.x = T) -> out_data

saveRDS(out_data,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds")
saveRDS(out_meta,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds")


# post 1960
dat_long <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/data_long_HN_HS.rds")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")

dat_month <- dat_long[, .(HN = sum(HN, na.rm = T),
                          HS = mean(HS, na.rm = T),
                          nn_HN = sum(!is.na(HN)),
                          nn_HS = sum(!is.na(HS)),
                          nn_in_month = days_in_month(Date[1])),
                      .(Name, year(Date), month(Date))]

dat_month[nn_HN < min_frac_avail * nn_in_month, HN := NA]
dat_month[nn_HS < min_frac_avail * nn_in_month, HS := NA]

out_data <- dat_month[!is.na(HS) | !is.na(HN), .(Name, year, month, HN, HS)]
out_meta <- dat_meta[Name %in% unique(out_data$Name)]

out_data[, .(year = min(year) : max(year)), .(Name)] %>% 
  .[, .(month = 1:12), .(Name, year)] %>% 
  merge(out_data, all.x = T) -> out_data

saveRDS(out_data,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds")
saveRDS(out_meta,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/meta_long_HN_HS.rds")






# gapfill -----------------------------------------------------------------

# only post 1960 and HS
# need to check limits first
# add info on fraction filled

dat_hs <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/04_GAPFILL/rds/gapfill-01-all.rds")
dat_hs


dat_month <- dat_hs[, .(HS = mean(HS, na.rm = T),
                        nn_HS = sum(!is.na(HS)),
                        nn_gapfill = sum(HS_fillcode == 222, na.rm = T),
                        nn_original = sum(HS_fillcode == 1, na.rm = T),
                        nn_in_month = days_in_month(Date[1])),
                    .(Name, year(Date), month(Date))]

dat_month[nn_HS < min_frac_avail * nn_in_month, HS := NA]

# remove more than 5 years before after original period
dat_month[, year_start := min(year[nn_original > 0]), .(Name)]
dat_month[, year_end := max(year[nn_original > 0]), .(Name)]

dat_month[year < (year_start - 5), HS := NA]
dat_month[year > (year_end + 5), HS := NA]


out_data <- dat_month[!is.na(HS), .(Name, year, month, HS, frac_filled = nn_gapfill / nn_in_month)]
out_meta <- dat_meta[Name %in% unique(out_data$Name)]

out_data[, .(year = min(year) : max(year)), .(Name)] %>% 
  .[, .(month = 1:12), .(Name, year)] %>% 
  merge(out_data, all.x = T) -> out_data

saveRDS(out_data,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020_gapfilled_HS/data_long_HS.rds")
saveRDS(out_meta,
        "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020_gapfilled_HS/meta_long_HS.rds")














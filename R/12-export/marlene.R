#
library(data.table)


dat <- rbind(
  readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds"),
  readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds")
)

dat_nn <- dat[, 
    .(n_hn = sum(!is.na(HN)),
        n_hs = sum(!is.na(HS))),
    .(Name, year)]
dat_nn2 <- dat_nn[ n_hn > 0 | n_hs > 0,
                   .(min_year = min(year)),
                   .(Name)]
dat_nn2[min_year < 1900]
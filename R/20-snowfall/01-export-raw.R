# export raw snowfall data

library(data.table)
library(magrittr)
library(fs)



# monthly -----------------------------------------------------------------

dat_meta_monthly <- unique(rbind(readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/meta_long_HN_HS.rds"),
                                 readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds")))

dat_monthly <- rbind(readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds"),
                     readRDS("~/projects/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds"))
dat_monthly[, HS := NULL]

# seasonal ----------------------------------------------------------------


# dat_meta_seasonal <- readRDS("~/projects/ALPINE_WIDE_SNOW/06_SEASONAL/Oct-Sep/meta_long_HN_HS.rds")
# 
# dat_seasonal <- readRDS("~/projects/ALPINE_WIDE_SNOW/06_SEASONAL/Oct-Sep/data_long_HN_HS.rds")




# write -------------------------------------------------------------------


dat_meta_out <- dat_meta_monthly
# dat_meta_out <- unique(rbind(dat_meta_monthly, dat_meta_seasonal))
setorder(dat_meta_out, Provider, Name)
setorder(dat_monthly, Name, year)
# setorder(dat_seasonal, Name, hydro_year)


fwrite(dat_meta_out, "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/snowfall-silvia/meta.csv")
fwrite(dat_monthly, "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/snowfall-silvia/data-monthly.csv")
# fwrite(dat_seasonal, "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/snowfall-silvia/data-seasonal.csv")



# extra: location of seasonal, not monthly --------------------------------

dat_check <- dat_meta_seasonal %>% dplyr::anti_join(dat_meta_monthly)
dat_check[, .N, Provider]







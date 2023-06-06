# meta information for excel table may-june 2023

library(data.table)
library(magrittr)
library(writexl)




# read --------------------------------------------------------------------

dat_long <- rbind(
  readRDS("~/projects/ALPINE_WIDE_SNOW/03_QC1/rds/1787hn_1879hs-1960/data_long_HN_HS.rds"),
  readRDS("~/projects/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/data_long_HN_HS.rds")
)

dat_meta <- unique(rbind(
  readRDS("~/projects/ALPINE_WIDE_SNOW/03_QC1/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds"),
  readRDS("~/projects/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")
))


# process -----------------------------------------------------------------

dat_info_HN <- dat_long[!is.na(HN), 
                        .(year_start = min(year(Date)),
                          year_end = max(year(Date))),
                        .(Name)]

dat_info_HS <- dat_long[!is.na(HS), 
                        .(year_start = min(year(Date)),
                          year_end = max(year(Date))),
                        .(Name)]

dat_info_HN[, variable := "HN"]
dat_info_HS[, variable := "HS"]

rbind(dat_info_HN, dat_info_HS) %>% 
  cbind(value = "YES") %>% 
  merge(dat_meta) %>% 
  dcast(... ~ variable, value.var = "value") -> dat_out

# dat_out[, HN := ifelse(is.na(HN), "NO", "YES")]
# dat_out[, HS := ifelse(is.na(HS), "NO", "YES")]
dat_out[is.na(HN), HN := "NO"]
dat_out[is.na(HS), HS := "NO"]
dat_out[, ID := paste0(Provider, "_", Name)]
dat_out[, freq := "daily"]
dat_out[, type := "manual"]
dat_out[, Provider := NULL]

setcolorder(dat_out,
            c("ID", "Name", "Longitude", "Latitude", "Elevation", 
              "year_start", "year_end", "freq", "type", "HS", "HN"))
dat_out

write_xlsx(dat_out, "data-raw/overview-meta.xlsx")

# need to manually check lonlat and elev decimal separator!

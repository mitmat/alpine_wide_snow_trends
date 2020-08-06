# make seasonal average 
# - from monthly
# - add extra coarse res data

library(data.table)
library(magrittr)
library(lubridate)
library(mitmatmisc)


# from daily - monthly ----------------------------------------------------


dat1 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds")
dat2 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds")
dat_all <- rbind(dat1, dat2)

meta1 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/meta_long_HN_HS.rds")
meta2 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/meta_long_HN_HS.rds")
meta_all <- unique(rbind(meta1, meta2))

add_hydro_year(dat_all)

dat_all_OctSep <- dat_all[, 
                          .(HN = sum(HN), HS = mean(HS), nn_month = .N),
                          .(Name, hydro_year)]
dat_all_OctSep <- dat_all_OctSep[nn_month == 12]

dat_all_NovMay <- dat_all[month %in% c(11,12,1:5), 
                          .(HN = sum(HN), HS = mean(HS), nn_month = .N),
                          .(Name, hydro_year)]
dat_all_NovMay <- dat_all_NovMay[nn_month == 7]


# add SMI -----------------------------------------------------------------

smi <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/smi/extra_monthly_HN.csv")
add_hydro_year(smi)

smi_OctSep <- smi[, 
                  .(HN = sum(hn), nn_month = .N),
                  .(Name = stn_name, hydro_year)]
smi_OctSep <- smi_OctSep[nn_month == 12]
ggplot(smi_OctSep, aes(hydro_year, HN, colour = Name))+geom_line()

dat_all_NovMay[Name %in% smi_OctSep$Name] %>% 
  ggplot(aes(hydro_year, HN, colour = Name))+geom_line()

# -> Domodossola not needed

add_smi_OctSep <- smi_OctSep[Name != "Domodossola_Collegio_Rosmini"]



# add giacomo -------------------------------------------------------------

gb_meta <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/giacomo/meta.csv",
                 encoding = "Latin-1")
gb_meta


gb_riva <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/giacomo/riva.csv")
add_hydro_year(gb_riva)
gb_riva_OctSep <- gb_riva[, 
                          .(HN = sum(hn), nn_month = .N, stn_id = "riva"),
                          .(hydro_year)]
gb_riva_OctSep <- gb_riva_OctSep[nn_month == 12]


gb_rovereto <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/giacomo/rovereto.csv")
add_hydro_year(gb_rovereto)
gb_rovereto_OctSep <- gb_rovereto[, 
                                  .(HN = sum(hn), nn_month = .N, stn_id = "rovereto"),
                                  .(hydro_year)]
gb_rovereto_OctSep <- gb_rovereto_OctSep[nn_month == 6] # -> only measuered Oct-Mar (but enough probably)


gb_trento <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/giacomo/trento_laste.csv")
gb_trento <- gb_trento[, .(hydro_year = season_year, HN = sum_hn, stn_id = "trento_laste")]

gb_all_OctSep <- rbindlist(list(
  gb_riva_OctSep, gb_rovereto_OctSep, gb_trento
), use.names = T, fill = T)

source("R/functions/safe_names.R")
gb_meta[, stn_name_safe := safe_names(stn_name)]
gb_all_OctSep %>% 
  merge(gb_meta[, .(stn_id, Name = stn_name_safe)], by = "stn_id") -> add_gb_OctSep

add_gb_meta <- gb_meta[stn_name_safe %in% add_gb_OctSep$Name]



# add HZB -----------------------------------------------------------------

hzb_meta <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/hzb/meta.csv")
hzb_data <- fread("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/06_SEASONAL/from-non-daily-data/hzb/data.csv")

hzb_data %>% summary
hzb_past_stns <- sort(unique(hzb_data$stn_name_safe))
dat_all_OctSep[Name %in% hzb_past_stns, .N, Name]
meta_all[Name %in% hzb_past_stns]
meta_all[Provider == "AT_HZB"]

# -> many stations in past HZB, which are not in daily data (and vice versa)

hzb_data[stn_name_safe %in% meta_all$Name] %>% summary
hzb_data[! stn_name_safe %in% meta_all$Name] %>% summary
# -> contains also data after 1970, try to merge and see if differences?




# merge all ---------------------------------------------------------------






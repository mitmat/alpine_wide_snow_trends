# export gapfill meteoswiss for PhD student cambridge

library(data.table)

dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")
dat_gapfill <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/04_GAPFILL/rds/gapfill-01-all.rds")


dat_meta_out <- dat_meta[Provider == "CH_METEOSWISS"]
dat_hs_out <- dat_gapfill[Name %in% dat_meta_out$Name]

fwrite(dat_meta_out, 
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/09_EXPORT/phd-cambridge/meta.csv")
fwrite(dat_hs_out, 
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/09_EXPORT/phd-cambridge/data.csv")

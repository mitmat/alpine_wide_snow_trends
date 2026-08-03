# write data 

library(data.table)
library(magrittr)
library(fs)
library(sf)
library(dplyr)
library(stringr)

path_out <- "~/projects/ALPINE_WIDE_SNOW/09_EXPORT/franz-fram3s/"

# read and prep data ---------------------------------------------------------------

dat_meta <- readRDS("~/projects/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1961-2020/r-data/meta_long_HN_HS.rds")

# subset to spatcons
stns_toremove <- readRDS("~/projects/ALPINE_WIDE_SNOW/07_SPATIAL_CONSISTENCY/rds/stns_to_remove.rds")

dat_meta <- dat_meta[!Name %in% stns_toremove]

sf_meta <- st_as_sf(dat_meta, coords = c("Longitude", "Latitude"), crs = 4326)
sf_bbox <-
  st_bbox(c(xmin = 9.6, xmax = 13.3, ymin = 45.5, ymax = 47.9)) |> 
  st_as_sfc() |> 
  st_as_sf(crs = 4326)

sf_meta_sub <- st_crop(sf_meta, sf_bbox)


dat_hs <- readRDS("~/projects/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/data_wide_HS.rds")

cols <- colnames(dat_hs)[-1]
cols_tokeep <- intersect(cols, sf_meta_sub$Name)

# dat_hs2 <- dat_hs[, c("Date", cols_tokeep), with = F]


# write data --------------------------------------------------------------------

for(i_col in cols_tokeep){
  
  dat_i <- dat_hs[, c("Date", i_col), with = F]
  setnames(dat_i, c("date", "hs"))
  dat_i2 <- dat_i[!is.na(hs)]
  
  fn_out <- path(path_out, i_col, ext = "csv")
  fwrite(dat_i2, fn_out)
  
}


# write meta --------------------------------------------------------------------

dat_stations <- dat_meta[, .(id = Name, provider = Provider, 
                             alt = Elevation, x = Longitude, y = Latitude)]

# check if stations.csv matches files

files_stn <- dir_ls(path_out) |> 
  path_file() |> 
  str_subset(fixed("stations.csv"), negate = T) |>
  path_ext_remove()

dat_stations <- dat_stations[id %in% files_stn]

files_stn[!files_stn %in% dat_stations$id]

stopifnot(all(files_stn %in% dat_stations$id))

fwrite(dat_stations,
       path(path_out, "stations.csv"))






# get MODIS data


# library(raster)
library(stars)
library(data.table)
library(lubridate)
library(magrittr)
library(fs)
library(ggplot2)
library(forcats)
library(scico)

all_files <- dir_ls("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/annual_SCD/")
#remove incomplete
all_files <- all_files[2:20]
all_files %>% path_file

all_files %>% 
  path_file %>% 
  substr(1,4) -> along_year

rs_all <- read_stars(all_files,
                     along = list(hydro_year = along_year))
rs_all
# plot(rs_all)

all_files %>% 
  path_file %>% 
  path_ext_remove() %>% 
  stringr::str_split("_", simplify = T) -> files_dates
new_names <- paste0(substr(files_dates[, 1], 1, 7),
                    "...",
                    substr(files_dates[, 2], 1, 7))

rs_all2 <- rs_all %>% st_set_dimensions("hydro_year", value = new_names)



# values clim, elevation, zone --------------------------------------------------------------------

rs_clim <- st_apply(rs_all, c("x", "y"), mean)
write_stars(rs_clim, "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif")





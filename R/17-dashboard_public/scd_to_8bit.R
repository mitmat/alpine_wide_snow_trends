# convert SCD 2000-10-01_2019-09-30 to 8bit for upload into mapbox
# 8bit value range 0 - 255
# SCD possible values: 0 - 365
# -> group into 2d intervals

# libs
library(stars)
library(sf)
library(dplyr)

# data
pth_scd = "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif"
scd = stars::read_stars(pth_scd)
# scd
# plot(scd)

# recalc the values
scd_2d = round(scd/2, 0)
# scd_2d_rcl = scd_2d + scd_2d

# check a bit
vals = scd %>% pull() %>% c()
vals_2d = scd_2d %>% pull() %>% c()
# vals_2d_rcl = scd_2d_rcl %>% pull() %>% c()
comp = tibble(vals = vals[!is.na(vals)], 
              vals_2d = vals_2d[!is.na(vals_2d)]) #, 
               #vals_2d_rcl = vals_2d_rcl[!is.na(vals_2d_rcl)])
comp
sort(unique(vals_2d))
hist(vals)
hist(vals_2d)


# save as 8bit tif
pth_out = paste0(dirname(pth_scd), "/", "2000-10-01_2019-09-30_8bit_scale2.tif")
stars::write_stars(obj = scd_2d, dsn = pth_out, type = "Byte", NA_value = 255)

# check it
scd_8bit = stars::read_stars(pth_out)
scd_8bit
plot(scd_8bit)
plot(scd_2d)
scd_2d
scd_8bit - scd_2d

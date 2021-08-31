# CONVERT SCD DATASETS FOR WMS SERVICE
# convert SCD 2000-10-01_2019-09-30 and other SCD products 
# to epsg:3035 and 16bit for upload into wms service

# libs
library(stars)
library(sf)
library(dplyr)

# data
pth_scd = list("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/2000-10-01_2019-09-30.tif", 
               "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean_2041_2070/rcp26_noglacier.tif", 
               "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean/rcp26_noglacier.tif", 
               "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean_2041_2070/rcp85_noglacier.tif", 
               "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean/rcp85_noglacier.tif")

pth_scd_new = list("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/clim_SCD/scd_20001001_20190930_16bit_3035.tif", 
                   "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean_2041_2070/scd_2041_2070_rcp26_noglacier_16bit_3035.tif", 
                   "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean/scd_2071_2100_rcp26_noglacier_16bit_3035.tif", 
                   "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean_2041_2070/scd_2041_2070_rcp85_noglacier_16bit_3035.tif", 
                   "/mnt/CEPH_PROJECTS/CLIRSNOW/data_bc/ds/scd_ensemble_mean/scd_2071_2100_rcp85_noglacier_16bit_3035.tif")

# warper fun
gdal_warper = function(in_file){
  message("warping: ", in_file)
  out_file = paste0(tools::file_path_sans_ext(in_file), "_16bit_3035_tmp.tif")
  gdal_cmd = paste0("gdalwarp -t_srs EPSG:3035 ", 
                    #"-ot UInt16 ", 
                    "-r bilinear ", 
                    "-overwrite ",
                    in_file, 
                    " ",  
                    out_file)
  system(gdal_cmd)
  return(out_file)
}

# apply warping and data type conversion
# in_file = pth_scd[[1]]
# pth_scd_new = lapply(pth_scd, function(in_file){
#   tmp_file = gdal_warper(in_file)
#   na_value = 65535
#   scd_3035 = read_stars(tmp_file) %>% round(0)
#   scd_3035[is.na(scd_3035[1])] = na_value
#   out_file = paste0(tools::file_path_sans_ext(in_file), "_16bit_3035.tif")
#   #out_file = gsub("-", "_", out_file)
#   write_stars(scd_3035, out_file, 
#               driver = "GTiff", update = FALSE, type = "UInt16", NA_value = na_value)
#   file.remove(tmp_file)
#   return(out_file)
# })

pth_scd_out = purrr::map2(.x = pth_scd, .y = pth_scd_new, .f = function(x, y){
  in_file = x
  tmp_file = gdal_warper(in_file)
  na_value = 65535
  scd_3035 = read_stars(tmp_file) %>% round(0)
  scd_3035[is.na(scd_3035[1])] = na_value
  out_file = y
  write_stars(scd_3035, out_file, 
              driver = "GTiff", update = FALSE, type = "UInt16", NA_value = na_value)
  file.remove(tmp_file)
  return(out_file)
})



# checks
scd = read_stars(pth_scd[[1]])
scd_new = read_stars(pth_scd_new[[1]])
st_crs(scd)
st_crs(scd_new)
system(paste0("gdalinfo ", pth_scd[[1]]))
system(paste0("gdalinfo ", pth_scd_new[[2]]))

summary(scd %>% pull() %>% c())
summary(scd_new %>% pull() %>% c())

plot(scd)
plot(scd_new)





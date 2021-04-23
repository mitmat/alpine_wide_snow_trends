# create shapes of N-S divide based on stn clustering

library(data.table)
library(magrittr)
library(mapedit)
library(sf)
library(raster)
library(mapview)
library(cluster)
library(foreach)



# prep data ---------------------------------------------------------------

load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")
dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

mat_clust3_eof <- sinkr_eof$u

# cluster and calc metrics ------------------------------------------------


dat_clust3_eof <- foreach(
  kk = 2:5,
  .final = rbindlist
) %do% {
  
  km_fit <- kmeans(mat_clust3_eof[, 1:kk], kk)
  km_sil <- silhouette(km_fit$cluster, dist(mat_clust3_eof[, 1:kk]))
  stn_clust <- km_fit$cluster
  data.table(#pca_k = pca_k,
    kk = kk, 
    cluster = stn_clust,
    Name = colnames(mat_eof),
    sil_width = km_sil[, 3],
    ss_tot = km_fit$totss,
    ss_withintot = km_fit$tot.withinss)
  
}


dat_clust3_eof[sil_width > 0.3] %>% 
  merge(dat_meta, by = "Name") %>% 
  ggplot(aes(Longitude, Latitude, colour = factor(cluster)))+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~kk)

sf_meta <- st_as_sf(dat_meta_clust,
                    coords = c("Longitude", "Latitude"),
                    crs = 4326)


rr <- raster("/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/05_temporal_complete_max10d/20020716_120000.tif")
# plot(rr)

mapview(sf_meta,
        zcol = "cluster_fct", 
        col.regions = scales::brewer_pal(palette = "Set1")) %>% 
  editMap() -> em_test 

em_test$finished[1, ] %>% 
  st_transform(st_crs(rr)) -> sf_crop

sf_crop %>% plot


mapview(list(rr, sf_crop))

saveRDS(sf_crop, "data/zones-manual-sf-v01.rds")

rr_zone <- rasterize(as(sf_crop, "Spatial"), rr)
rr_zone[is.na(rr_zone)] <- 2L
plot(rr_zone)

sp2 <- drawPoly()

rr_zone2 <- rasterize(sp2, rr_zone, field = 1, update = T)
rr_zone2 %>% plot

saveRDS(rr_zone2, file = "data/zones-manual-raster-v01.rds")
writeRaster(rr_zone2, filename = "data/zones-manual-v01.tif")

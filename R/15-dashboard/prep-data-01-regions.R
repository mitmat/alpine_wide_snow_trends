# try to cluster the stations?
# based on daily series or on first 5 PCs



library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
library(scico)
library(lemon)
library(cluster)
library(sinkr)
library(foreach)



# prep data ---------------------------------------------------------------

load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/03_QC1/rds/1961-2020/meta_long_HN_HS.rds")

mat_clust3_eof <- sinkr_eof$u

# cluster and calc metrics ------------------------------------------------


dat_clust3_eof <- foreach(
  kk = 2:5,
  .final = rbindlist
) %do% {

  km_fit <- kmeans(mat_clust3_eof[, 1:kk], kk)
  # km_sil <- silhouette(km_fit$cluster, dist(mat_clust3_eof[, 1:pca_k]))
  stn_clust <- km_fit$cluster
  data.table(#pca_k = pca_k,
             kk = kk, 
             cluster = stn_clust,
             Name = colnames(mat_eof),
             #sil_width = km_sil[, 3],
             ss_tot = km_fit$totss,
             ss_withintot = km_fit$tot.withinss)
  
}

saveRDS(dat_clust3_eof,
        file = "data/dash/clusters.rds")

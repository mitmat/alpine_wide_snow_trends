# plot of clusters



library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
# library(foreach)
library(scico)
library(lemon)
library(patchwork)
library(directlabels)



# eof ---------------------------------------------------------------------



dat_meta_cluster <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/meta-with-cluster-01.rds")

gg_clust <-
  dat_meta_cluster %>% 
  ggplot(aes(Longitude, Latitude, colour = cluster_fct, shape = cluster_fct))+
  borders()+
  geom_point(size = 1)+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_quickmap(xlim = range(dat_meta_cluster$Longitude), ylim = range(dat_meta_cluster$Latitude))+
  theme_bw()+
  theme(legend.position = c(0.7, 0.1),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.direction = "horizontal")+
  scale_color_brewer(palette = "Set1")

ggsave(gg_clust,
       filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/fig/Figure 4.png",
       width = 7, height = 4)



# pca full ----------------------------------------------------------------




dat_meta_cluster2 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/meta-with-cluster-02-pca-full.rds")

gg_clust2 <-
  dat_meta_cluster2 %>% 
  ggplot(aes(Longitude, Latitude, colour = cluster_fct, shape = cluster_fct))+
  borders()+
  geom_point(size = 1)+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_quickmap(xlim = range(dat_meta_cluster2$Longitude), ylim = range(dat_meta_cluster2$Latitude))+
  theme_bw()+
  theme(legend.position = c(0.7, 0.1),
        legend.title = element_blank(),
        legend.background = element_rect(colour = "black"),
        legend.direction = "horizontal")+
  scale_color_brewer(palette = "Set1")

ggsave(gg_clust2,
       filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/fig/Figure A6.png",
       width = 7, height = 4)


# test --------------------------------------------------------------------

dat_meta_cluster %>% 
  ggplot(aes(Longitude, Elevation, colour = cluster_fct))+
  geom_point()+
  scale_color_brewer(palette = "Set1")+
  theme_bw()

dat_meta_cluster %>% 
  with(table(Provider, cluster_fct))
  


# compare clustering eof vs pca -------------------------------------------

dat_comp_clust <- merge(
  dat_meta_cluster[, .(Name, cluster_eof = cluster_fct)],
  dat_meta_cluster2[, .(Name, cluster_pca = cluster_fct)]
)

dat_comp_clust %>% with(table(cluster_eof, cluster_pca))
dat_comp_clust[, sum(cluster_eof == cluster_pca) / .N]
dat_comp_clust[, sum(cluster_eof == cluster_pca)]


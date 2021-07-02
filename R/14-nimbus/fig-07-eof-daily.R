# EOF analysis


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


# prep data ---------------------------------------------------------------


load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

data.table(Name = colnames(mat_eof), sinkr_eof$u) %>% 
  merge(dat_meta, by = "Name") -> dat_sinkr_eof

dat_sinkr_eof[, country := substr(Provider, 1, 2)]
setnames(dat_sinkr_eof, paste0("V", 1:20), paste0("PC", 1:20))
dat_sinkr_eof %>% 
  melt(id.vars = c("country", "Provider", "Name", "Longitude", "Latitude", "Elevation", "cluster_fct"),
       measure.vars = paste0("PC", 1:5)) -> dat_sinkr_plot

dat_sinkr_plot[, value_sc := scales::rescale(value, c(-1,1)), variable]
sinkr_eof_summary[, prop_sd_format := scales::percent(prop_sd, .1)]
sinkr_eof_summary[, facet_lbl := paste0(pc, " (", prop_sd_format, ")")]
pc_rename <- setNames(sinkr_eof_summary$facet_lbl, sinkr_eof_summary$pc)
dat_sinkr_plot[, facet_lbl := pc_rename[variable]]


gg_eof <-
dat_sinkr_plot[variable %in% paste0("PC", 1:4)] %>% 
  ggplot(aes(Longitude, Latitude, colour = value_sc))+
  borders()+
  geom_point(size = 0.5)+
  scale_color_scico("Scaled loadings", palette = "roma",
                    guide = guide_colorbar(title.position = "top", direction = "horizontal"))+
  facet_wrap(~facet_lbl)+

  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"), breaks = c(5,10,15))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_quickmap(xlim = range(dat_meta$Longitude), ylim = range(dat_meta$Latitude))+
  theme_bw()+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())


ggsave(gg_eof,
       filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/pca-sub4.jpg",
       width = 6, height = 4)

# ggsave(gg_eof2,
#        filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/fig-pdf/Figure 3.pdf",
#        width = 8, height = 4)




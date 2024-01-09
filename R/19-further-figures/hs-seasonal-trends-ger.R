# plot trends (full long period)


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
library(patchwork)

library(forcats)
library(foreach)
# library(flextable)
# library(officer)

pval_sym <- function(pval){
  symnum(pval, corr = FALSE,
         cutpoints = c(0,  .001,.01,.05, .1, 1),
         symbols = c("***","**","*","."," "))
}

dat_meta_clust <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

load("~/alps-snow/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")




# winter ------------------------------------------------------------------

dat_plot_djf <- dat_plot_full[variable %in% c("meanHS_DJF")]

dat_plot_djf[, elev_fct := cut(Elevation, breaks = c(0, 1000, 2000, 3000), dig.lab = 4)]
dat_plot_djf_n <- dat_plot_djf[, 
                               .(pos = sum(estimate >=0) / .N, 
                                 neg = sum(estimate < 0) / .N), 
                               .(elev_fct)]
dat_plot_djf_n[, elev_n := mitmatmisc::num_cut_fct(elev_fct)]


gg1 <- dat_plot_djf %>% 
  ggplot(aes(estimate*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  # geom_linerange(alpha = 0.3, aes(xmin = est_low*10, xmax = est_high*10))+
  # facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_color_brewer("", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[cm pro Dekade]")+
  ylab("Höhe [m]")


# gg1 <- 
dat_plot_djf %>% 
  ggplot(aes(estimate*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  # geom_linerange(alpha = 0.3, aes(xmin = est_low*10, xmax = est_high*10))+
  # facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_color_brewer("", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[cm pro Dekade]")+
  ylab("Höhe [m]")

gg2 <- dat_plot_djf %>% 
  ggplot(aes(trend.rel*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  # geom_linerange(alpha = 0.3, aes(xmin = est_low*10, xmax = est_high*10))+
  # facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_x_continuous(labels = scales::percent_format())+
  scale_color_brewer("", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[% pro Dekade]")+
  ylab("Höhe [m]")


(gg1 + gg2)+
  plot_annotation(title = "Trend mittlere Schneehöhe Winter (Dez-Feb)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave("fig/trends-djf.png",
       width = 7, height = 4)



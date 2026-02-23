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

dat_meta_clust <- readRDS("~/projects/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

load("~/projects/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")



dat_plot_full <- dat_seasonal_gls[term == "year0" & !startsWith(variable, "SCD")] %>% 
  merge(dat_meta_clust, by = "Name")
dat_plot_full[, est_low := estimate - 1.96 * std.error]
dat_plot_full[, est_high := estimate + 1.96 * std.error]
dat_plot_full[, variable_fct := fct_relevel(factor(variable), "maxHS_NDJFMAM", after = Inf)]
dat_plot_full

# winter ------------------------------------------------------------------

dat_plot_djf <- dat_plot_full[variable %in% c("meanHS_DJF")]

dat_plot_djf[, elev_fct := cut(Elevation, breaks = c(0, 1000, 2000, 3000), dig.lab = 4)]
dat_plot_djf_n <- dat_plot_djf[, 
                               .(pos = sum(estimate >=0) / .N, 
                                 neg = sum(estimate < 0) / .N), 
                               .(elev_fct)]
dat_plot_djf_n[, elev_n := mitmatmisc::num_cut_fct(elev_fct)]
dat_plot_djf_n2 <- dat_plot_djf_n %>% 
  melt(measure.vars = c("pos", "neg"))
dat_plot_djf_n2[, xx := ifelse(variable == "pos", 
                               max(dat_plot_djf$estimate),
                               min(dat_plot_djf$estimate))]

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
  xlab("[cm per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)


gg1n <-
dat_plot_djf %>% 
  ggplot()+
  geom_point(aes(estimate*10, Elevation), size = 0.5, colour = "black")+
  geom_tile(data = dat_plot_djf_n, 
            aes(0, elev_n, width = Inf, height = 1000, fill = elev_fct),
            alpha = 0.7)+
  geom_label(data = dat_plot_djf_n2, 
             aes(10*xx/2, elev_n, fill = elev_fct, label = scales::label_percent(1)(value)),
             alpha = 1)+
  geom_vline(xintercept = 0)+
  # scale_fill_distiller(palette = "YlGnBu", aesthetics = c("colour", "fill"))+
  scale_fill_brewer(palette = "Pastel2", aesthetics = c("colour", "fill"))+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[cm per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)

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
  xlab("[% per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)


(gg1 + gg2)+
  plot_annotation(title = "Trend average winter snow depth (Dec-Feb)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave("fig/trends-djf_EN.png",
       width = 7, height = 4)

# (gg1n + gg2)+
#   plot_annotation(title = "Trend mittlere Schneehöhe Winter (Dez-Feb)",
#                   theme = theme(plot.title = element_text(size = 16, face = "bold")))
# 
# ggsave("fig/trends-djf-numbers.png",
#        width = 7, height = 4)




# spring ------------------------------------------------------------------

dat_plot_mam <- dat_plot_full[variable %in% c("meanHS_MAM")]

dat_plot_mam[, elev_fct := cut(Elevation, breaks = c(0, 1000, 2000, 3000), dig.lab = 4)]
dat_plot_mam_n <- dat_plot_mam[, 
                               .(pos = sum(estimate >=0) / .N, 
                                 neg = sum(estimate < 0) / .N), 
                               .(elev_fct)]
dat_plot_mam_n[, elev_n := mitmatmisc::num_cut_fct(elev_fct)]
dat_plot_mam_n2 <- dat_plot_mam_n %>% 
  melt(measure.vars = c("pos", "neg"))
dat_plot_mam_n2[, xx := ifelse(variable == "pos", 
                               max(dat_plot_mam$estimate),
                               min(dat_plot_mam$estimate))]

gg1 <- dat_plot_mam %>% 
  ggplot(aes(estimate*10, Elevation, colour = cluster_fct))+
  geom_vline(xintercept = 0)+
  geom_point(size = 0.5)+
  # geom_linerange(alpha = 0.3, aes(xmin = est_low*10, xmax = est_high*10))+
  # facet_grid(. ~ cluster_fct2)+ # free_x or free
  scale_color_brewer("", palette = "Set1")+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[cm per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)


gg1n <-
  dat_plot_mam %>% 
  ggplot()+
  geom_point(aes(estimate*10, Elevation), size = 0.5, colour = "black")+
  geom_tile(data = dat_plot_mam_n, 
            aes(0, elev_n, width = Inf, height = 1000, fill = elev_fct),
            alpha = 0.7)+
  geom_label(data = dat_plot_mam_n2, 
             aes(10*xx/2, elev_n, fill = elev_fct, label = scales::label_percent(1)(value)),
             alpha = 1)+
  geom_vline(xintercept = 0)+
  # scale_fill_distiller(palette = "YlGnBu", aesthetics = c("colour", "fill"))+
  scale_fill_brewer(palette = "Pastel2", aesthetics = c("colour", "fill"))+
  theme_bw(14)+
  theme(panel.grid.minor = element_blank(),
        legend.position = "none")+ # 
  xlab("[cm per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)

gg2 <- dat_plot_mam[10*trend.rel < 0.25] %>% 
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
  xlab("[% per decade]")+
  ylab("Elevation [m]")+
  ylim(0,3000)


(gg1 + gg2)+
  plot_annotation(title = "Trend average spring snow depth (Mar-May)",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave("fig/trends-mam_EN.png",
       width = 7, height = 4)

# (gg1n + gg2)+
#   plot_annotation(title = "Trend mittlere Schneehöhe Frühling (Mär-Mai)",
#                   theme = theme(plot.title = element_text(size = 16, face = "bold")))
# 
# ggsave("fig/trends-mam-numbers.png",
#        width = 7, height = 4)

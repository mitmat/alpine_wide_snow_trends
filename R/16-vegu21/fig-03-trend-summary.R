
library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(broom)

# elev_int <- 1000
elev_int <- 500




# table start and end -----------------------------------------------------

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")




dat_seasonal %>% 
  merge(dat_meta_clust, by = "Name") -> dat_plot_ts
dat_plot_ts[, elev_fct := cut(Elevation, breaks = seq(0, 3000, by = elev_int), dig.lab = 5)]
dat_plot_ts[, ns_fct := fct_collapse(cluster_fct,
                                     "North" = c("NE", "NW", "North & high Alpine"),
                                     "South" = c("South & high Alpine", "SE"))]


dat_plot_ts[, value_stn_mean := mean(value), .(Name, variable)]
dat_plot_ts[, value_anomaly := value - value_stn_mean]

dat_plot_ts_mean <- dat_plot_ts[, 
                                .(mean_value = mean(value),
                                  mean_value_anomaly = mean(value_anomaly),
                                  nn = .N),
                                .(year, variable, elev_fct, ns_fct)]


dat_plot_ts_mean[, year0 := year - min(year)]
dat_plot_ts_mean[nn > 5,
                 tidy(lm(mean_value ~ year0)),
                 .(variable, elev_fct, ns_fct, nn)] %>% 
  dcast(variable + elev_fct + ns_fct + nn ~ term, value.var = "estimate") -> dat_table_start_end
setnames(dat_table_start_end, c("variable", "elevation", "region", "nn" ,"year1971", "trend"))

dat_table_start_end[, year2019 := year1971 + 49*trend]


# plot HS --------------------------------------------------------------------


# dat_plot <- dat_table_start_end[variable != "maxHS_NDJFMAM"]
dat_plot <- dat_table_start_end[startsWith(variable, "meanHS") & 
                                  !(variable %in% c("meanHS_MAM", "meanHS_NDJFMAM") & elevation == "(0,500]")]
dat_plot[, variable := fct_inorder(substr(variable, 8, 99))]
# dat_plot[, c("snow_index", "season") := tstrsplit(variable, "_")]

dat_plot[, rel_change := (year2019-year1971) / year1971]

dat_plot %>% 
  melt(measure.vars = c("year1971", "year2019"),
       variable.name = "year") -> dat_plot2
dat_plot2[, year := as.numeric(substr(year, 5, 9))]

dat_label_elev <- dat_plot2[year == 1971,
                            .(ypos = mean(value)),
                            .(year, variable, elevation)] 
dat_label_elev[, elevation_lbl := paste0(elevation, "m")]

dat_label_perc <- dat_plot2[year == 2019,
                            .(year, value, variable, elevation, region, rel_change)]
dat_label_perc[, lbl := scales::percent(rel_change, 1)]
dat_label_perc[, year := ifelse(region == "North", 2019, 2050)]

dat_label_perc_title <- dat_label_perc[, 
                                       .(value = max(value)),
                                       .(region, year)]
dat_label_perc_title[, value_max := max(value)]

# label number of stations

dat_plot2 %>% 
  ggplot(aes(year, value, colour = region, linetype = elevation))+
  geom_point()+
  geom_line(aes(group = paste0(region, elevation)))+
  geom_text(data = dat_label_perc_title,
            aes(label = region, y = value_max, linetype = NULL),
            hjust = -0.2, vjust = -1)+
  geom_text(data = dat_label_perc,
            aes(label = lbl), hjust = -0.2)+
  geom_text(data = dat_label_elev,
            aes(y = ypos, colour = NULL, label = elevation_lbl), hjust = 1.1)+
  scale_x_continuous(limits = c(1900, 2100))+
  scale_color_brewer(palette = "Dark2")+
  facet_grid(. ~ variable)+
  theme_bw(14)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Change 1971-2019 (from linear trend)")+
  ylab("mean HS [cm]")


ggsave("fig/trend-summary-vegu21-HS.png",
       width = 10, height = 5)



# plot SCD --------------------------------------------------------------------


dat_plot <- dat_table_start_end[startsWith(variable, "SCD")]
# dat_plot <- dat_table_start_end[startsWith(variable, "meanHS") & 
                                  # !(variable %in% c("meanHS_MAM", "meanHS_NDJFMAM") & elevation == "(0,500]")]
dat_plot[, variable := factor(substr(variable, 5, 99),
                              levels = c("NDJF", "MAM", "NDJFMAM"))]
# dat_plot[, c("snow_index", "season") := tstrsplit(variable, "_")]

dat_plot[, rel_change := (year2019-year1971) / year1971]

dat_plot %>% 
  melt(measure.vars = c("year1971", "year2019"),
       variable.name = "year") -> dat_plot2
dat_plot2[, year := as.numeric(substr(year, 5, 9))]

dat_label_elev <- dat_plot2[year == 1971,
                            .(ypos = mean(value)),
                            .(year, variable, elevation)] 
dat_label_elev[, elevation_lbl := paste0(elevation, "m")]

dat_label_perc <- dat_plot2[year == 2019,
                            .(year, value, variable, elevation, region, rel_change)]
dat_label_perc[, lbl := scales::percent(rel_change, 1)]
dat_label_perc[, year := ifelse(region == "North", 2019, 2050)]

dat_label_perc_title <- dat_label_perc[, 
                                       .(value = max(value)),
                                       .(variable, region, year)]
dat_label_perc_title[, value_max := max(value), .(variable)]

# label number of stations

dat_plot2 %>% 
  ggplot(aes(year, value, colour = region, linetype = elevation))+
  geom_point()+
  geom_line(aes(group = paste0(region, elevation)))+
  geom_text(data = dat_label_perc_title,
            aes(label = region, y = value_max, linetype = NULL),
            hjust = -0.2, vjust = -1)+
  geom_text(data = dat_label_perc,
            aes(label = lbl), hjust = -0.2)+
  geom_text(data = dat_label_elev,
            aes(y = ypos, colour = NULL, label = elevation_lbl), hjust = 1.1)+
  scale_x_continuous(limits = c(1900, 2100))+
  scale_color_brewer(palette = "Dark2")+
  facet_wrap( ~ variable, scales = "free_y")+
  theme_bw(14)+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())+
  xlab("Change 1971-2019 (from linear trend)")+
  ylab("mean SCD [days]")


ggsave("fig/trend-summary-vegu21-SCD.png",
       width = 11, height = 5)



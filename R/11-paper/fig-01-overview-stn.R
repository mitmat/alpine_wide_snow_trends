# figure overview 
# - map
# - altitude distribution
# - time with # (maybe divide by country)



library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
library(directlabels)
library(patchwork)

library(forcats)
library(foreach)
library(flextable)
library(officer)

library(fs)



# data --------------------------------------------------------------------

# hn paper
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/data-longterm-HN-HS.rda")
stns_hn_used <- dat_meta_hn$Name
dat_meta_hn_used <- copy(dat_meta_hn)


# hs paper
dat_meta_paper <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/07_SPATIAL_CONSISTENCY/data-to-check/meta_wide_HS.rds") 
stns_hs_used <- dat_meta_paper$Name
dat_meta_hs_used <- copy(dat_meta_paper)

# subset to spatcons
stns_ok <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/spatcons_stns_ok.rds")
stns_hs_used <- stns_hs_used[stns_hs_used %in% stns_ok]
dat_meta_hs_used <- dat_meta_hs_used[Name %in% stns_ok]



# meta all
dat_meta_hn <- unique(rbind(
  readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1961-2020/r-data/meta_wide_HN.rds"),
  readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1787hn_1879hs-1960/r-data/meta_wide_HN.rds")
))

dat_meta_hs <- unique(rbind(
  readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1961-2020/r-data/meta_wide_HS.rds"),
  readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1787hn_1879hs-1960/r-data/meta_wide_HS.rds")
))

dat_meta_all <- unique(rbind(dat_meta_hn, dat_meta_hs))

# remove Giacomo's data (until no answer)
# dat_meta_all[, .N, Provider]
# dat_meta_all <- dat_meta_all[Provider != "IT_TN_GB"]
# dat_meta_hs <- dat_meta_hs[Provider != "IT_TN_GB"]
# dat_meta_hn <- dat_meta_hn[Provider != "IT_TN_GB"]

# time series
dat_daily_hnhs_1 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1787hn_1879hs-1960/r-data/data_long_HN_HS.rds")
dat_daily_hnhs_2 <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/02_JOIN_REGIONS/1961-2020/r-data/data_long_HN_HS.rds")
dat_daily_hnhs <- rbind(dat_daily_hnhs_1, dat_daily_hnhs_2)

# time series summary
dat_ts_hn <- dat_daily_hnhs[!is.na(HN), .(n_stn = length(unique(Name))), .(Provider, year(Date))]
dat_ts_hs <- dat_daily_hnhs[!is.na(HS), .(n_stn = length(unique(Name))), .(Provider, year(Date))]




# plot --------------------------------------------------------------------

cols_2grey <- setNames(scales::grey_pal()(2),
                       c("used", "available"))


# ** map ------------------------------------------------------------------

dat_plot_map <- rbindlist(idcol = "ff", list(
  "HN available" = dat_meta_hn,
  "HS available" = dat_meta_hs,
  "HN used" = dat_meta_hn_used,
  "HS used" = dat_meta_hs_used
))

dat_plot_map[, hnhs := substr(ff, 1, 2)]
dat_plot_map[, used := substr(ff, 4, 99)]


gg_map <- dat_plot_map %>% 
  ggplot(aes(Longitude, Latitude, shape = hnhs, colour = used))+
  borders()+
  geom_point()+
  scale_shape_manual("", values = c(HN = 1, HS = 4))+
  scale_colour_manual("", values = cols_2grey)+
  xlab(NULL)+ylab(NULL)+
  scale_x_continuous(labels = function(x) paste0(x, "° E"))+
  scale_y_continuous(labels = function(x) paste0(x, "° N"))+
  coord_quickmap(xlim = range(dat_meta_all$Longitude), ylim = range(dat_meta_all$Latitude))+
  theme_bw()+
  theme(legend.position = c(0.8, 0.2),
        legend.direction = "horizontal",
        legend.background = element_rect(colour = "black"))





# ** elev -----------------------------------------------------------------

dat_meta_all$Elevation %>% summary

gg_elev <- dat_meta_hs %>% 
  ggplot(aes(Elevation))+
  geom_histogram(breaks = seq(0, 3300, by = 50), aes(fill = "available"))+
  geom_histogram(breaks = seq(0, 3300, by = 50),
                 data = dat_meta_hs_used,
                 aes(fill = "used"))+
  geom_point(data = dat_meta_hn_used, aes(y = -5, colour = "used"), shape = 1)+
  scale_fill_manual("HS", values = cols_2grey)+
  scale_colour_manual("HN", values = cols_2grey)+
  theme_bw()+
  theme(legend.position = c(0.8, 0.6),
        legend.direction = "vertical",
        # legend.title = element_blank(),
        legend.background = element_rect(colour = "black"))+
  xlab("Elevation [m]")+
  ylab("Number of stations")




# ** ts HN and HS -------------------------------------------------------------------

cols_country <- c(scales::brewer_pal(type = "qual")(6), "black")
cols_country <- c(scales::hue_pal()(6), "black")
lty_country <- c(rep("solid", 6), "dashed")
xlim_common <- range(c(dat_ts_hn$year, dat_ts_hs$year))


dat_ts_hn[, country := substr(Provider, 1, 2)]
dat_plot_ts_hn <- rbind(use.names = T,
                          dat_ts_hn[, .(n_stn = sum(n_stn)), .(year, country)],
                          dat_ts_hn[, .(n_stn = sum(n_stn), country = "Total"), .(year)])
  
  
gg_ts_hn <- dat_plot_ts_hn %>% 
  ggplot(aes(year, n_stn, colour = country, linetype = country))+
  geom_line()+
  scale_color_manual("", values = cols_country)+
  scale_linetype_manual("", values = lty_country)+
  xlim(xlim_common)+
  theme_bw()+
  xlab(NULL)+
  ylab("Number of HN stations")

# -> maybe only one ts summary over all countries
# -> maybe not

# gg_ts_hn_dl <- direct.label(gg_ts_hn, "extreme.grid")
# gg_ts_hn_dl <- direct.label(gg_ts_hn, "first.bumpup")
gg_ts_hn_dl <- direct.label(gg_ts_hn, "top.bumpup")



dat_ts_hs[, country := substr(Provider, 1, 2)]
dat_plot_ts_hs <- rbind(use.names = T,
                        dat_ts_hs[, .(n_stn = sum(n_stn)), .(year, country)],
                        dat_ts_hs[, .(n_stn = sum(n_stn), country = "Total"), .(year)])


gg_ts_hs <- dat_plot_ts_hs %>% 
  ggplot(aes(year, n_stn, colour = country, linetype = country))+
  geom_line()+
  scale_color_manual("", values = cols_country)+
  scale_linetype_manual("", values = lty_country)+
  xlim(xlim_common)+
  theme_bw()+
  xlab(NULL)+
  ylab("Number of HS stations")

# gg_ts_hs_dl <- direct.label(gg_ts_hs, "extreme.grid")
# gg_ts_hs_dl <- direct.label(gg_ts_hs, "first.bumpup")
gg_ts_hs_dl <- direct.label(gg_ts_hs, "top.bumpup")


# combine and save --------------------------------------------------------

# gg_map + gg_elev + gg_ts_hn_dl + gg_ts_hs_dl + plot_layout(nrow = 2)
# (gg_map / gg_elev) | (gg_ts_hn_dl / gg_ts_hs_dl)

gg_out_1 <- gg_map + gg_elev + plot_annotation(tag_levels = "a", 
                                               tag_suffix = ")",
                                               tag_prefix = "(")

ggsave(gg_out_1,
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/fig/Figure 1.png",
       width = 12, height = 4)

gg_out_2 <- gg_ts_hn_dl / gg_ts_hs_dl


ggsave(gg_out_2,
       file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/fig/Figure 2.png",
       width = 4, height = 3, scale = 2)



# ** ts only one (not used) ----------------------------------------------------------
# 
# dat_ts_hn[, country := substr(Provider, 1, 2)]
# dat_ts_hs[, country := substr(Provider, 1, 2)]
# 
# dat_plot_ts1 <- rbind(use.names = T,
#                         dat_ts_hn[, .(snow = "HN", n_stn = sum(n_stn), country = "Total"), .(year)],
#                         dat_ts_hs[, .(snow = "HS", n_stn = sum(n_stn), country = "Total"), .(year)])
# 
# dat_plot_ts1 %>% 
#   ggplot(aes(year, n_stn, linetype = snow))+
#   geom_line()+
#   theme_bw()



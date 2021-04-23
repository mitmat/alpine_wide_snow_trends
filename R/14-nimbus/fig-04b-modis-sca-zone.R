# create plots of stats

library(ggplot2)
library(magrittr)
library(data.table)

source("R/14-nimbus/read_stats.R")




# v1.1 --------------------------------------------------------------------


fn_stats <- "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/stats-regions-NS/05_temporal_complete_max10d.csv"
fn_sample_raster <- "/mnt/CEPH_PROJECTS/SNOW_3rd_gen/CLOUDREMOVAL/v1.1/05_temporal_complete_max10d/20020716_120000.tif"


dat <- read_stats_zone(fn_stats, fn_sample_raster)


# fill to complete MODIS period (2000-2020), hydro-year style
data.table(date = seq(ymd("1999-10-01"), ymd("2020-09-30"), by = "day")) %>% 
  merge(dat[value_fct == "snow"], 
        by = "date", all.x = T) -> dat_plot
mitmatmisc::add_hydro_year(dat_plot)
dat_plot[, hydro_year_label := paste0(hydro_year - 1, "-", hydro_year)]
dat_plot[, region_fct := fct_recode(factor(region),
                                    "Sud" = "1", "Nord" = "2")]

month_eng_ita <- setNames(c("Gen", "Feb", "Mar", "Apr", "Mag","Giu", 
                            "Iug", "Ago", "Set", "Ott", "Nov", "Dic"),
                          month.abb)
f_custom_label <- function(x){
  month_abbr_eng <- month(x, label = T, abbr = T)
  month_eng_ita[month_abbr_eng]
}

gg_sca <-
  dat_plot[date >= "2000-10-01" & !is.na(region_fct)] %>% 
  ggplot(aes(date, area_km2/1000, colour = fct_rev(region_fct)))+
  geom_line(na.rm = T)+
  scale_color_brewer("", palette = "Dark2", direction = 1)+
  # scale_x_date(date_minor_breaks = "1 month", date_labels = "%b", expand = c(0,0))+
  scale_x_date(date_minor_breaks = "1 month", expand = c(0,0),
               labels = f_custom_label)+
  # scale_x_date(date_minor_breaks = "1 month", # date_breaks = "3 months",
               # labels = c("Oct", "Gen", "Apr", "Iug"), expand = c(0,0))+
  facet_wrap(~hydro_year_label, scales = "free_x")+
  theme_bw()+
  theme(panel.grid.minor.y = element_blank())+
  xlab(NULL)+
  ylab(expression(paste("Superficie coperta da neve [1000 ", km^2, "]")))

ggsave(gg_sca,
       filename = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/ts_modis_sca_zone.png",
       width = 12,
       height = 8)





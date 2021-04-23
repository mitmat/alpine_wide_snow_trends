# plots of ts with 500m averages

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
library(patchwork)

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
# dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")

level_fct_hs <- c("meanHS_DJF", "meanHS_MAM", "meanHS_NDJFMAM", "maxHS_NDJFMAM")
level_fct_scd <- c("SCD_NDJF", "SCD_MAM", "SCD_NDJFMAM")

# make complete
expand.grid(Name = sort(unique(dat_seasonal$Name)),
            year = 1971:2019, 
            variable = c(level_fct_hs, level_fct_scd), 
            stringsAsFactors = F) %>% 
  data.table() %>% 
  merge(dat_seasonal, all.x = T) -> dat_full

all_stn <- sort(unique(dat_full$Name))

pdf("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/fig/zenodo-SM/ts-stn-seasonal-all.pdf",
    width = 14, height = 7)
for(i_stn in all_stn){
  
  tit <- dat_meta_clust[Name == i_stn,
                        paste0(Name, " (", Provider, "), ", Elevation, "m, ",
                               round(Longitude, 3), "°E ", round(Latitude, 3), "°N")]
  
  dat_i <- dat_full[Name == i_stn]
  dat_i_HS <- dat_i[!startsWith(variable, "SCD")]
  dat_i_HS[, variable_fct := factor(variable, levels = level_fct_hs)]
  dat_i_SCD <- dat_i[startsWith(variable, "SCD")]
  dat_i_SCD[, variable_fct := factor(variable, levels = level_fct_scd)]

  
  gg1 <-
  ggplot(dat_i_HS, aes(year, value))+
    geom_point(na.rm = T)+
    facet_grid(. ~ variable_fct)+
    scale_x_continuous(labels = c("'70", "'80", "'90", "2000", "'10", "'20"))+
    ylim(0, NA)+
    theme_bw()+
    xlab(NULL)+
    ylab("Seasonal mean HS [cm]")+
    ggtitle(tit)
  
  gg2 <-
    ggplot(dat_i_SCD, aes(year, value))+
    geom_point(na.rm = T)+
    facet_grid(. ~ variable_fct)+
    scale_x_continuous(labels = c("'70", "'80", "'90", "2000", "'10", "'20"))+
    ylim(0, NA)+
    theme_bw()+
    xlab(NULL)+
    ylab("Seasonal mean SCD [days]")

  
  gg_out <- gg1 / gg2  
  print(gg_out)
}
dev.off()

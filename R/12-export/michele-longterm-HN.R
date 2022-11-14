# long term HN series for michele

library(data.table)
library(magrittr)
library(ggplot2)


# monthly -----------------------------------------------------------------


dat_meta <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/meta_long_HN_HS.rds")

dat <- rbind(
  readRDS("~/alps-snow/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1787hn_1879hs-1960/data_long_HN_HS.rds"),
  readRDS("~/alps-snow/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020/data_long_HN_HS.rds")
)
  
dat_year <- dat[!is.na(HN),
                .(year_min = min(year),
                  year_max = max(year),
                  year_nn = length(unique(year))),
                .(Name)]
dat_year2 <- merge(dat_year, dat_meta)



# seasonal ----------------------------------------------------------------


dat <- readRDS("~/alps-snow/ALPINE_WIDE_SNOW/06_SEASONAL/Oct-Sep/data_long_HN_HS.rds")

dat_year <- dat[!is.na(HN),
                .(year_min = min(hydro_year),
                  year_max = max(hydro_year),
                  year_nn = length(unique(hydro_year))),
                .(Name)]
dat_year2 <- merge(dat_year, dat_meta)

dat_100 <- dat_year2[year_nn >= 100]


all_stn <- sort(dat_100$Name)
year_lim <- range(dat_100[Name != "Torino", c(year_min, year_max)])

pdf("fig/michele-longterm-HN/ts-seasonal.pdf", width = 8, height = 4)
for(i_stn in all_stn){
  
  dat_i <- dat[Name == i_stn]
  
  title <- 
  dat_100[Name == i_stn,
          paste0(Name, " (", Provider, "), ", Elevation, "m, ",
                 round(Longitude, 2), "°E ", round(Latitude, 2), "°N")]
  
  gg <- ggplot(dat_i, aes(hydro_year, HN))+
    geom_point()+
    theme_bw()+
    xlim(year_lim)+
    ylim(0, NA)+
    ggtitle(title)+
    xlab(NULL)+ylab("Seasonal sum of HN (Oct-Sep)")
  
  print(gg)
  
}
dev.off()


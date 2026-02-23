library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)
library(patchwork)



# random ------------------------------------------------------------------


load("~/projects/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")

# which(colnames(mat_eof) == "Arabba")
# mat_eof[, 88] |> plot(type = "l")

set.seed(1234)
stn_sub <- sample(1:ncol(mat_eof), 12)

gg <- mat_eof[, stn_sub] |> 
  as.data.table() |> 
  cbind(ii = 1:nrow(mat_eof)) |> 
  melt(id.vars = "ii") |> 
  ggplot(aes(ii, value))+
  geom_line()+
  facet_wrap(~variable, scales = "free_y")+
  theme_bw()+
  xlab(NULL)+
  ylab("snow depth [cm]")

ggsave(filename = "fig/some-HS-time-series.png",
       gg, width = 16, height = 8)



# arabba detail -----------------------------------------------------------

dat_hs <- readRDS("~/projects/ALPINE_WIDE_SNOW/04_GAPFILL/rds/gapfill-01-all.rds")
dat1 <- dat_hs[Name == "Arabba"]
dat1[, month := month(Date)]
dat1[, month_fct := mitmatmisc::month_fct(month, 10)]
dat1[, year := year(Date)]
mitmatmisc::add_hydro_year(dat1)

dat1_seas <- dat1[month %in% c(11:12, 1:5) & !is.na(HS), 
                  .(hs = mean(HS), nn = .N),
                  hydro_year]

dat1_month <- dat1[month %in% c(11:12, 1:5) & !is.na(HS), 
                  .(hs = mean(HS), nn = .N),
                  .(month, month_fct, hydro_year)]


dat1_seas[hydro_year > 1961 & hydro_year < 2020] |> 
  ggplot(aes(hydro_year, hs))+
  geom_point()+
  theme_bw()+
  xlab(NULL)+
  ylab("Seasonal average snow depth (Nov-May) [cm]")

ggsave("fig/arabba-novmay.png", width = 6, height = 4)


dat1_seas[hydro_year > 1961 & hydro_year < 2020] |> 
  ggplot(aes(hydro_year, hs))+
  geom_point()+
  geom_smooth(method = lm, se = F, aes(colour = "lm/ols"))+
  scale_color_discrete("")+
  theme_bw()+
  xlab(NULL)+
  ylab("Seasonal average snow depth (Nov-May) [cm]")

ggsave("fig/arabba-novmay-lm.png", width = 6, height = 4)

dat1_seas[hydro_year > 1961 & hydro_year < 2020] |> 
  ggplot(aes(hydro_year, hs))+
  geom_point()+
  geom_smooth(method = loess, se = F, aes(colour = "loess"))+
  geom_smooth(method = lm, se = F, aes(colour = "lm/ols"))+
  scale_color_discrete("")+
  theme_bw()+
  xlab(NULL)+
  ylab("Seasonal average snow depth (Nov-May) [cm]")

ggsave("fig/arabba-novmay-lm-loess.png", width = 6, height = 4)


# dat1_seas[hydro_year > 1961 & hydro_year < 2020] |> 
#   ggplot(aes(hydro_year, hs))+
#   geom_point()+
#   # geom_smooth(method = lm, formula = y ~ poly(x,1), se = F, aes(colour = "y ~ poly(x,1)"))+
#   # geom_smooth(method = lm, formula = y ~ poly(x,2), se = F, aes(colour = "y ~ poly(x,2)"))+
#   # geom_smooth(method = lm, formula = y ~ poly(x,3), se = F, aes(colour = "y ~ poly(x,3)"))+
#   geom_smooth(method = loess, se = F, aes(colour = "loess"))+
#   theme_bw()+
#   xlab(NULL)+
#   ylab("Seasonal average snow depth (Nov-May) [cm]")



dat1_month[hydro_year > 1961 & hydro_year < 2020 & month != 5] |> 
  ggplot(aes(hydro_year, hs))+
  geom_point()+
  # geom_smooth(method = lm, se = F, aes(colour = "lm/ols"))+
  # scale_color_discrete("")+
  facet_wrap(~month_fct, scales = "free_y")+
  theme_bw()+
  xlab(NULL)+
  ylab("Monthly average snow depth [cm]")

ggsave("fig/arabba-monthly.png", width = 12, height = 6)

dat1_month[hydro_year > 1961 & hydro_year < 2020 & month != 5] |> 
  ggplot(aes(hydro_year, hs))+
  geom_point()+
  geom_smooth(method = lm, se = F)+
  facet_wrap(~month_fct, scales = "free_y")+
  theme_bw()+
  xlab(NULL)+
  ylab("Monthly average snow depth [cm]")

ggsave("fig/arabba-monthly-lm.png", width = 12, height = 6)

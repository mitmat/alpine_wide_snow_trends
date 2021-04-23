# trends


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
# library(nlme)
library(broom)
# library(broom.mixed)
library(trend)

# prep data ---------------------------------------------------------------

dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")

setorder(dat_month, Name, month, year)
setorder(dat_seasonal, Name, variable, year)

# for month calendar year, for seasonal hydro year, but same name
dat_month[, year0 := year - min(year)]
dat_seasonal[, year0 := year - min(year)]

setnames(dat_month, "HS", "value")


# SEN ---------------------------------------------------------------------

f_sen <- function(dat){
  if(length(unique(dat$value)) <= 2) return(NULL)
  
  sen1 <- sens.slope(dat$value)
  dat_sen <- tidy(sen1)

  data.table(dat_sen,
             estimate = sen1$estimates)
}

dat_month_sen <- dat_month[, f_sen(.SD), .(Name, month)]
dat_seasonal_sen <- dat_seasonal[, f_sen(.SD), .(Name, variable)]


# save --------------------------------------------------------------------



save(dat_month_sen, dat_seasonal_sen, 
     file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-02-1971-2019-sen.rda")




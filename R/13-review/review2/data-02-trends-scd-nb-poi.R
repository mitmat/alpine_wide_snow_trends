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
# library(trend)
library(MASS)

# prep data ---------------------------------------------------------------

# dat_month <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-01-monthly.rds")
dat_seasonal <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/data-02-seasonal.rds")
dat_seasonal <- dat_seasonal[startsWith(variable, "SCD")]

# setorder(dat_month, Name, month, year)
setorder(dat_seasonal, Name, variable, year)

# for month calendar year, for seasonal hydro year, but same name
# dat_month[, year0 := year - min(year)]
dat_seasonal[, year0 := year - min(year)]

# setnames(dat_month, "HS", "value")


# NB ---------------------------------------------------------------------

f_nb <- function(dat){
  if(length(unique(dat$value)) <= 2) return(NULL)
  
  glm1 <- glm.nb(value ~ year0, data = dat)
  dat_cf <- tidy(glm1)
  dat_summ <- glance(glm1)
  rsq <- 1 - sum(resid(glm1, type = "response")^2) / sum((dat$value - mean(dat$value))^2)
  
  predict(glm1, data.frame(year0 = c(0,49)), type = "response") %>% 
    diff() -> trend_abs
  
  glm2 <- glm(value ~ year0, data = dat, family = "poisson")
  pval_glm <- pchisq(2* (logLik(glm1) - logLik(glm2)), df = 1, lower.tail = F)
  
  data.table(dat_cf, 
             r.squared = rsq,
             dat_summ, 
             resid.sd = sd(resid(glm1, type = "response")^2),
             p.value.nb.poisson = pval_glm,
             trend.abs = trend_abs)
}

# dat_month_sen <- dat_month[, f_sen(.SD), .(Name, month)]
dat_seasonal_scd_nb <- dat_seasonal[, f_nb(.SD), .(Name, variable)]



# poisson ---------------------------------------------------------------------

f_poi <- function(dat){
  if(length(unique(dat$value)) <= 2) return(NULL)
  
  glm2 <- glm(value ~ year0, data = dat, family = "poisson")
  dat_cf <- tidy(glm2)
  dat_summ <- glance(glm2)
  rsq <- 1 - sum(resid(glm2, type = "response")^2) / sum((dat$value - mean(dat$value))^2)
  predict(glm2, data.frame(year0 = c(0,49)), type = "response") %>% 
    diff() -> trend_abs
  
  
  data.table(dat_cf, 
             r.squared = rsq,
             dat_summ, 
             resid.sd = sd(resid(glm2, type = "response")^2),
             trend.abs = trend_abs)
}

# dat_month_sen <- dat_month[, f_sen(.SD), .(Name, month)]
dat_seasonal_scd_poi <- dat_seasonal[, f_poi(.SD), .(Name, variable)]




# save --------------------------------------------------------------------



save(dat_seasonal_scd_nb, dat_seasonal_scd_poi,
     file = "/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-03-1971-2019-scd.rda")




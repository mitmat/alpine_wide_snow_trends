# comparison ols vs nb, poisson

library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(forcats)
library(foreach)
library(patchwork)

library(flextable)
library(officer)

pval_sym <- function(pval){
  symnum(pval, corr = FALSE,
         cutpoints = c(0,  .001,.01,.05, .1, 1),
         symbols = c("***","**","*","."," "))
}

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-01-1971-2019-ols-gls.rda")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-02-1971-2019-sen.rda")
load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/trends-03-1971-2019-scd.rda")

dat_seasonal_scd_nb[term == "year0", 
                    .(Name, variable, est_nb = estimate, pval_nb = p.value, est_nb_abs = trend.abs)] %>% 
  merge(dat_seasonal_scd_poi[term == "year0", 
                             .(Name, variable, est_poi = estimate, pval_poi = p.value, est_poi_abs = trend.abs)]) %>% 
  merge(dat_seasonal_ols[term == "year0" & startsWith(variable, "SCD"), 
                         .(Name, variable, est_ols = estimate, pval_ols = p.value, est_ols_rel = trend.rel)] ) %>%
  merge(dat_seasonal_sen[startsWith(variable, "SCD"), 
                         .(Name, variable, est_sen = estimate, pval_sen = p.value)] ) %>% 
  merge(dat_meta_clust) -> dat_comp



dat_seasonal_gls[term == "year0", 
                 .(Name, variable, est_gls = estimate, pval_gls = p.value)] %>% 
  merge(dat_seasonal_ols[term == "year0", 
                         .(Name, variable, est_ols = estimate, pval_ols = p.value)] ) %>%
  merge(dat_seasonal_sen[, 
                         .(Name, variable, est_sen = estimate, pval_sen = p.value)] ) %>% 
  merge(dat_meta_clust) -> dat_comp2


dat_month_gls[term == "year0", 
              .(Name, month, est_gls = estimate, pval_gls = p.value)] %>% 
  merge(dat_month_ols[term == "year0", 
                      .(Name, month, est_ols = estimate, pval_ols = p.value)] ) %>%
  merge(dat_month_sen[, 
                      .(Name, month, est_sen = estimate, pval_sen = p.value)] ) %>% 
  merge(dat_meta_clust) -> dat_comp3


dat_comp[, elev_fct := cut(Elevation, breaks = seq(0,3000,by=500), dig.lab=5)]
dat_comp2[, elev_fct := cut(Elevation, breaks = seq(0,3000,by=500), dig.lab=5)]
dat_comp3[, elev_fct := cut(Elevation, breaks = seq(0,3000,by=500), dig.lab=5)]

# comp poi-nb -------------------------------------------------------------



dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_poi, est_nb))+
  geom_point()+
  facet_wrap(~variable)


with(dat_comp, table(nb = pval_sym(pval_nb), poi = pval_sym(pval_poi)))
with(dat_comp, table(ols = pval_sym(pval_ols), poi = pval_sym(pval_poi)))
with(dat_comp, table(ols = pval_sym(pval_ols), nb = pval_sym(pval_nb)))


# -> nb looks better!



# comp poi/nb vs ols ------------------------------------------------------




dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_ols, exp(est_nb)))+
  geom_point()+
  facet_wrap(~variable)


dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_ols, est_nb))+
  geom_point()+
  facet_grid(variable ~ elev_fct, scales = "free")

with(dat_comp, table(ols = pval_sym(pval_ols), nb = pval_sym(pval_nb), elev_fct))



dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_ols_rel, exp(est_nb)-1))+
  geom_point()+
  facet_wrap(~variable)
  # facet_grid(variable ~ elev_fct, scales = "free")

dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_ols_rel*49, exp(est_nb*49)-1))+
  # ggplot(aes(est_ols_rel, est_nb))+
  geom_abline()+
  geom_point()+
  facet_wrap(~variable)
  # facet_grid(variable ~ elev_fct, scales = "free")



# full trends
dat_comp %>% 
# dat_comp[est_nb > -0.1] %>% 
  ggplot(aes(est_ols*49, est_nb_abs))+
  # ggplot(aes(est_ols_rel, est_nb))+
  geom_abline()+
  geom_point()+
  # facet_wrap(~variable)
  facet_grid(variable ~ elev_fct, scales = "free")


# sen vs ols --------------------------------------------------------------

with(dat_comp2, table(gls = pval_sym(pval_gls), sen = pval_sym(pval_sen)))
with(dat_comp2, table(gls = pval_sym(pval_gls), sen = pval_sym(pval_sen), variable))

with(dat_comp2, table(ols = pval_sym(pval_ols), sen = pval_sym(pval_sen)))
with(dat_comp2, table(ols = pval_sym(pval_ols), sen = pval_sym(pval_sen), variable))

dat_comp2 %>% 
  ggplot(aes(est_gls, est_sen))+
  geom_abline()+
  geom_point()+
  facet_wrap(~variable, scales = "free")
  # facet_grid(variable ~ elev_fct, scales = "free")


dat_comp2[!startsWith(variable, "SCD"),
          .(bias = 10*mean(est_gls - est_sen),
            mae = 10*mean(abs(est_gls - est_sen)),
            corr = cor(est_gls, est_sen)),
          .(variable)]



# gls vs ols (SCD) --------------------------------------------------------

with(dat_comp2[startsWith(variable, "SCD")], 
     table(ols = pval_sym(pval_ols), gls = pval_sym(pval_gls)))

dat_comp2[startsWith(variable, "SCD")] %>% 
  ggplot(aes(est_gls, est_ols))+
  geom_abline()+
  geom_point()+
  facet_wrap(~variable, scales = "free")



# sen gls month -----------------------------------------------------------

dat_comp3 %>% 
  ggplot(aes(est_gls, est_sen))+
  geom_abline()+
  geom_point()+
  facet_wrap(~month, scales = "free")

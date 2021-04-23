# try average series per cluster


library(data.table)
library(lubridate)
library(magrittr)
library(ggplot2)
library(scico)

dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/meta-with-cluster-01.rds")

load("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/02_review/rds/regions-01-sinkr-eof.rda")

sinkr_eof %>% str
mat_eof %>% str

mat_test <- sinkr_eof$A %*% t(sinkr_eof$u)
# plot(mat_test[1:100000], scale(mat_eof)[1:100000])
# cor(mat_test[1:100000], scale(mat_eof)[1:100000], use = "p")


mat_test %>% str
colnames(mat_test) <- colnames(mat_eof)

data.table(date = mat_eof_dates, mat_test) %>% 
  melt(id.vars = c("date"), variable.factor = F, variable.name = "Name") -> dat_series_eof

dat_series_eof %>% 
  merge(dat_meta, by = "Name") -> dat_series_eof2


dat_summ <- dat_series_eof2[, 
                            .(mean_value = mean(value)),
                            .(cluster_fct, month = month(date), day = mday(date))]

dat_summ[, date := make_date(2000, month, day)]
dat_summ[month == 12, date := make_date(1999, month, day)]
dat_summ %>% summary
dat_summ %>% 
  ggplot(aes(date, mean_value))+
  geom_line()+
  facet_wrap(~cluster_fct)+
  theme_bw()

dat_summ %>% 
  ggplot(aes(date, mean_value, colour = cluster_fct))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  # facet_wrap(~cluster_fct)+
  theme_bw()



dat_hs <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/04_GAPFILL/rds/gapfill-01-all.rds")
setnames(dat_hs, "Date", "date")
dat_hs <- mitmatmisc::add_hydro_year(dat_hs)
dat_hs_sub <- dat_hs[hydro_year >= 1981 & hydro_year <= 2010 &
                       month(date) %in% c(12, 1:4)]

# make anomalies
dat_hs_sub[, month := month(date)]
dat_hs_sub[, day := mday(date)]
dat_hs_sub[, HS_anom := (HS - mean(HS))/sd(HS), .(Name, month, day)]
dat_hs_sub %>% 
  merge(dat_meta, by = "Name") %>% 
  .[!is.na(HS),
    .(mean_HS = mean(HS)),
    .(cluster_fct, month, day)] -> dat_summ_hs

dat_summ_hs[, date := make_date(2000, month, day)]
dat_summ_hs[month == 12, date := make_date(1999, month, day)]

dat_summ_hs %>% 
ggplot(aes(date, mean_HS))+
  geom_line()+
  facet_wrap(~cluster_fct)+
  theme_bw()

dat_summ_hs %>% 
  ggplot(aes(date, mean_HS, colour = cluster_fct))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  # facet_wrap(~cluster_fct)+
  theme_bw()
 


# eof ts ------------------------------------------------------------------

dat_summ_eofts <-  dat_series_eof2[,
                                   .(mean_value = mean(value)),
                                   .(date, cluster_fct)]
mitmatmisc::add_hydro_year(dat_summ_eofts)

dat_summ_eofts %>% 
  ggplot(aes(date, mean_value, colour = cluster_fct))+
  geom_line()+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~hydro_year, scales = "free_x")+
  theme_bw()

ggsave("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/NIMBUS/fig/zz_cluster-eof-ts.png",
       width = 16, height = 8)


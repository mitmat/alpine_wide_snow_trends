# graphical abstract? (full long period)


library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scico)

library(forcats)
library(foreach)
library(flextable)
library(officer)




# try trends based on season ----------------------------------------------

month_sub <- c(11, 12, 1:5)

dat_meta_clust <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/meta-with-cluster-01.rds")
dat_meta <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020_gapfilled_HS_sub/meta_long_HS.rds")
dat_hs <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/05_MONTHLY/rds/1961-2020_gapfilled_HS_sub/data_long_HS.rds")

dat_hs <- mitmatmisc::add_hydro_year(dat_hs)
mitmatmisc::add_season_fct(dat_hs)

# subset to spatcons
stns_ok <- readRDS("/mnt/CEPH_PROJECTS/ALPINE_WIDE_SNOW/PAPER/rds/spatcons_stns_ok.rds")
dat_meta <- dat_meta[Name %in% stns_ok]
dat_hs <- dat_hs[Name %in% stns_ok]


# subset to BZ
dat_hs_sub <- dat_hs[Name %in% dat_meta[Provider == "IT_BZ", Name]]
# dat_hs_sub <- dat_hs[Name %in% dat_meta[Provider == "CH_SLF", Name]]

with(dat_hs_sub, table(year, month))
with(dat_hs_sub[frac_gapfilled < 1], table(year, month))

dat_hs2 <- dat_hs_sub[year >= 1981 & year <= 2018 & month %in% month_sub & !is.na(HS)]
with(dat_hs2, table(year, month))

dat_hs2[, nn_year := .N, .(Name, month)]
dat_hs_full <- dat_hs2[nn_year == max(nn_year)]

dat_meta[Name %in% dat_hs_full$Name, .N, Provider]


# lm
dat_hs_full[, year0 := year - min(year)]
dat_hs_full_lm <- dat_hs_full[,
                              broom::tidy(lm(HS ~ year0)), 
                              .(Name, month)]




# test plot ------------------------------------------------------

dat_lm_full <- copy(dat_hs_full_lm)


dat_lm_full[!is.na(statistic)] %>% 
  dcast(Name + month ~ term, value.var = "estimate") %>% 
  .[! (abs(`(Intercept)`) < 1 & abs(year0) < 0.05)] %>% 
  .[, .(Name, month)] %>% 
  merge(dat_lm_full) -> dat_lm_sub




dat_lm_sub %>% 
  dcast(Name + month ~ term, value.var = "estimate") -> dat_plot

setnames(dat_plot, "(Intercept)", "y1981")

dat_plot[, y2018 := y1981 + 37*year0]
dat_plot[, y_dif := 37*year0]

dat_plot2 <- melt(dat_plot, 
                  id.vars = c("Name", "month"),
                  measure.vars = c("y1981", "y2018"))

mitmatmisc::add_month_fct(dat_plot2, 10)

dat_plot2 <- dat_plot2 %>% merge(dat_meta, by = "Name")

# dat_plot2[, value_sc := value / max(value), .(elev_fct, season)]

dat_plot2 %>% 
  ggplot(aes(month_fct, value, fill = variable))+
  geom_col(position = "identity")+
  facet_wrap(~fct_reorder(Name, Elevation), scales = "free_y")+
  theme_bw()

# puh?!

mitmatmisc::add_month_fct(dat_plot, 10)
dat_plot <- merge(dat_plot, dat_meta, by = "Name")

dat_plot[Name == "Anterivo_Osservatore"] %>% 
  ggplot(aes(month_fct, fill = factor(sign(y_dif))))+
  geom_tile(aes(y = y1981/2, height = y1981, width = 1), 
            fill = NA, colour = "grey20", size = 1)+
  geom_tile(aes(y = y1981 + y_dif/2, height = abs(y_dif), width = 1))+
  geom_tile(aes(y = y2018/2, height = y2018, width = 1), 
            fill = NA, colour = "black", size = 1)+
  theme_bw()
  

dat_plot %>% 
  ggplot(aes(month_fct, fill = factor(sign(y_dif))))+
  geom_tile(aes(y = y1981/2, height = y1981, width = 1), 
            fill = "#92c5de", colour = "grey20", size = 1)+
  geom_tile(aes(y = y1981 + y_dif/2, height = abs(y_dif), width = 1))+
  geom_tile(aes(y = y2018/2, height = y2018, width = 1), 
            fill = NA, colour = "black", size = 1)+
  scale_fill_manual(values = c("#e41a1c", "#377eb8"))+
  facet_wrap(~fct_reorder(Name, Elevation), scales = "free_y")+
  theme_bw()




dat_plot %>% 
  ggplot(aes(month_fct, fill = factor(sign(y_dif))))+
  geom_tile(aes(y = y1981/2, height = y1981, width = 1), 
            fill = "#92c5de")+
  geom_tile(aes(y = y1981 + y_dif/2, height = abs(y_dif), width = 1))+
  geom_tile(aes(y = y2018/2, height = y2018, width = 1), 
            fill = NA)+
  scale_fill_manual(values = c("#e41a1c", "#377eb8"))+
  facet_wrap(~fct_reorder(Name, Elevation), scales = "free_y")+
  theme_bw()


# rect: dashed 1981, solid 2018, the larger one outside, smaller inside (does not matter +-)
# more faint colors red and blue. only diff in color, no color baseline?, maybe better!




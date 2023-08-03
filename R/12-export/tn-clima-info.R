# data and fig for snow trento clima

library(ggplot2)
library(magrittr)
library(data.table)
library(forcats)
library(flextable)
library(officer)

load("data/future-summary-country-elev-500m.rda")
# load("data/future-summary-country-elev-200m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

# remove some countries
country_remove <- c("Hungary", "Liechtenstein", "San Marino", "Slovakia")

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600 & ! country %in% country_remove,
                       .(scd = mean(scd), nn = .N),
                       .(country, elev_f, elev, experiment, period, fp)]

# dat_ens_mean <- dat_bc[elev > 200 & elev < 3600 & ! country %in% country_remove,
#                        .(scd = mean(snc)*365),
#                        .(country, elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n2020" = "2001-2020",
                                      "2041\n2070" = "2041-2070",
                                      "2071\n2100" = "2071-2100")]
dat_ens_mean[, elev_fct := fct_inorder(paste0(elev_f, "m"))]

cols <- scales::brewer_pal(palette = "YlOrBr")(5)[c(3,5)]

gg <- dat_ens_mean[country == "Italy" & elev < 3000] %>% 
  ggplot(aes(period_f, scd, colour = experiment))+
  geom_path(aes(group = experiment))+
  geom_point()+
  scale_color_manual("Scenario", values = cols)+
  facet_grid(~ elev_fct)+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab(NULL)+
  ylab("Snow cover duration in Italy [days]")

ggsave("fig/country/tn-clima-italy.png", 
       gg, width = 9, height = 4)

# table -------------------------------------------------------------------


dat_table <- dat_ens_mean[country == "Italy" & elev < 3000] 

dat_table2 <- dat_table %>% 
  dcast(country + experiment + elev_fct ~ period, value.var = "scd")

ft <- dat_table2 %>% 
  flextable() %>% 
  colformat_double(digits = 0) %>% 
  autofit()

save_as_docx(ft, path = "fig/country/tn-clima-italy.docx")




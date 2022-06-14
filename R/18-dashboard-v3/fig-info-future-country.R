# future summary by country 


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
                       .(scd = mean(scd)),
                       .(country, elev_f, elev, experiment, period, fp)]

# dat_ens_mean <- dat_bc[elev > 200 & elev < 3600 & ! country %in% country_remove,
#                        .(scd = mean(snc)*365),
#                        .(country, elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n-\n2020" = "2001-2020",
                                      "2041\n-\n2070" = "2041-2070",
                                      "2071\n-\n2100" = "2071-2100")]


dat_ens_mean[period != "2041-2070"] %>% 
  dcast(country + elev ~ experiment + fp, value.var = "scd") -> dat_lollipop

dat_lollipop[, elev_fct := fct_inorder(paste0(elev, " m"))]
dat_lollipop[, country_fct := factor(country)]

rect_width <- 0.2

# cols <- setNames(c("#3182bd", "#de2d26", "#fee090", grey(0.7)),
#                  c("rcp26", "rcp85", "loss", "anno_month"))
cols <- setNames(c("#377eb8", "#e41a1c", "#ff7f00", grey(0.7), grey(0)),
                 c("rcp26", "rcp85", "loss", "anno_month", "black"))



# plot --------------------------------------------------------------------

# dat_plot <- dat_lollipop[elev %in% c(500, 1500, 2500, 3500)]

dy <- 300


dat_anno <- dat_lollipop[elev == 1500 & country == "Bosnia and Herzegovina"]
# dat_anno_repel <- cbind(dat_anno,
#                         yy = c(3500, 1500 + dy, 1500, 1500 - dy),
#                         colour = c("black", "rcp26", "rcp85", "loss"),
#                         label = c("recent SCD (2001-2020)",
#                                   "reduction in future SCD with 1.5-2°C warming",
#                                   "reduction in future SCD with 4-5°C warming",
#                                   "SCD saved with climate action"))

dat_anno_repel <- cbind(dat_anno,
                        yy = 100 + c(1500 + 3.5*dy, 1500, 1500 - 3.5*dy),
                        xx = c(167, 120, 120) - 10,
                        colour = c("rcp26", "rcp85", "loss"),
                        label = c("... with 1.5-2°C warming",
                                  "reduction in future SCD with 4-5°C warming",
                                  "SCD saved with climate action"),
                        hjust = c(0, 0, 0))


gg <- dat_lollipop[elev %in% c(500, 1500, 2500, 3500)] %>% 
  ggplot()+
  
  geom_vline(xintercept = 0:12*30, colour = cols["anno_month"])+ # 1 month
  
  geom_vline(aes(xintercept = rcp26_past))+
  
  geom_point(aes(x = rcp26_future, y = elev + dy), colour = cols["rcp26"])+
  geom_segment(aes(x = rcp26_future, xend = rcp26_past, y = elev + dy, yend = elev + dy), colour = cols["rcp26"])+
  
  geom_rect(aes(xmin = rcp85_future, xmax = rcp26_future,
                ymin = elev - dy - dy*rect_width, ymax = elev - dy + dy*rect_width),
            fill = cols["loss"])+
  
  geom_point(aes(x = rcp85_future, y = elev), colour = cols["rcp85"])+
  geom_segment(aes(x = rcp85_future, xend = rcp26_past, y = elev, yend = elev), colour = cols["rcp85"])+
  
  
  # annotation
  geom_text(aes(x = rcp26_past, y = 100, label = elev_fct), hjust = -0.1, vjust = 0, size = 3)+
  
  
  # month
  geom_text(data = data.frame(country_fct = factor("Croatia", levels = levels(dat_lollipop$country_fct))),
            x = 315, y = 1000, label = "(~1 month)", colour = cols["anno_month"], size = 2.5)+
  geom_text(data = data.frame(country_fct = factor("Croatia", levels = levels(dat_lollipop$country_fct))),
            x = 315, y = 3000, label = "30 days", colour = cols["anno_month"], size = 2.5)+
  geom_segment(data = data.frame(country_fct = factor("Croatia", levels = levels(dat_lollipop$country_fct))),
               x = 300, xend = 330, y = 2000, yend = 2000, colour = cols["anno_month"],
               arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "in")))+
  
  
  # legend
  geom_label(data = dat_anno,
             aes(x = rcp26_past, y = 4000), 
             hjust = 0, vjust = 1, size = 2.5, label.padding = unit(0.15, "lines"),
             label = "recent SCD (2001-2020)")+
  geom_label(data = dat_anno_repel,
             aes(x = xx, y = yy, colour = colour, label = label, hjust = hjust),
             size = 2.5, label.padding = unit(0.15, "lines"))+
  
  
  # other
  scale_x_continuous(limits = c(0, 380), expand = c(0, 0), breaks = 0:4*90)+
  # scale_y_continuous(breaks = 1:3, labels = c("RCP2.6 (1.5 - 2°C)",
  #                                             "loss due to delay / no action",
  #                                             "RCP8.5 (4 - 5°C)"),
  #                    limits = c(0,4))+
  scale_y_continuous(breaks = c(500, 1500, 2500, 3500),
                     labels = paste0(c(500, 1500, 2500, 3500), "m"),
                     limits = c(0, 4000), expand = c(0,0))+
  scale_colour_manual(values = cols, guide = "none")+
  facet_grid(country_fct ~ ., as.table = T, switch = "y", drop = T)+
  cowplot::theme_cowplot()+
  # theme_bw()+
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_line(colour = cols["anno_month"]),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        plot.background = element_rect(colour = "white", fill = "white"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0))+
  
  ylab(NULL)+
  xlab("Snow cover duration (SCD) [days]")+
  ggtitle("Snow cover duration in the Alps",
          "Impact of global warming and climate action on end of century (2071-2100) snow cover")

ggsave(gg, filename = "fig/country/info-future-country_EN.png", width = 9, height = 6)



## german ------------------------------------------------------------------

## italian ------------------------------------------------------------------

## french ------------------------------------------------------------------

## spanish? ------------------------------------------------------------------




# table -------------------------------------------------------------------


dat_table <- dat_lollipop[elev %in% c(500, 1500, 2500, 3500)]



## rcp26 -------------------------------------------------------------------


dat_table[, value_abs := rcp26_future - rcp26_past]
dat_table[, value_rel := (rcp26_future - rcp26_past) / rcp26_past]
dat_table[, label := sprintf("%2.0f d (%2.0f%%)", round(value_abs), round(value_rel*100, 1))]
dat_table2 <- dcast(dat_table, country_fct ~ elev_fct, value.var = "label")

ft_rcp26 <- dat_table2 %>% 
  flextable() %>% 
  set_header_labels("country_fct" = "") %>% 
  set_caption("Global warming 1.5-2°C (RCP2.6)") %>% 
  autofit()


## rcp85 -------------------------------------------------------------------


dat_table[, value_abs := rcp85_future - rcp26_past]
dat_table[, value_rel := (rcp85_future - rcp26_past) / rcp26_past]
dat_table[, label := sprintf("%2.0f d (%2.0f%%)", round(value_abs), round(value_rel*100, 1))]
dat_table2 <- dcast(dat_table, country_fct ~ elev_fct, value.var = "label")

ft_rcp85 <- dat_table2 %>% 
  flextable() %>% 
  set_header_labels("country_fct" = "") %>% 
  set_caption("Global warming 4-5°C (RCP8.5)") %>% 
  autofit()



## print -------------------------------------------------------------------

read_docx() %>% 
  body_add_flextable(ft_rcp26) %>% 
  body_add_par("") %>% body_add_par("") %>% 
  body_add_flextable(ft_rcp85) %>% 
  print(target = "fig/country/table-info-future-country.docx")
  



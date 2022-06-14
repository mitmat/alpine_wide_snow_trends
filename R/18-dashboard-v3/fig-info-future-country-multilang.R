# future summary by country 


library(ggplot2)
library(magrittr)
library(data.table)
library(forcats)
# library(flextable)
# library(officer)



# country names -----------------------------------------------------------


dat_country_names <- tibble::tribble(
  ~EN, ~DE, ~IT, ~FR,
  "Austria", "Österreich", "Austria", "Autriche",
  "Bosnia and Herzegovina", "Bosnien und Herzegowina", "Bosnia ed Erzegovina", "Bosnie-Herzégovine",
  "Croatia", "Kroatien", "Croazia", "Croatie",
  "France", "Frankreich", "Francia", "France",
  "Germany", "Deutschland", "Germania", "Allemagne",
  "Italy", "Italien", "Italia", "Italie",
  "Slovenia", "Slowenien", "Slovenia", "Slovénie",
  "Switzerland", "Schweiz", "Svizzera", "Suisse"
) %>% data.table()

dat_country_names[, country_fct := EN]

dat_country_names <- dat_country_names[, lapply(.SD, factor)]

# data prep ---------------------------------------------------------------


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



dy <- 300

dat_anno <- dat_lollipop[elev == 1500 & country == "Bosnia and Herzegovina"]

dat_lollipop <- merge(dat_lollipop, dat_country_names, by = "country_fct")

# plot function --------------------------------------------------------------------

f_plot <- function(lang = "EN", 
                   # country_anno_month = "Croatia",
                   lbl_month = "month",
                   lbl_days = "days",
                   lbl_recent = "recent SCD (2001-2020)",
                   lbl_xlab = "Snow cover duration (SCD) [days]",
                   lbl_title = "Snow cover duration in the Alps",
                   lbl_subtitle = "Impact of global warming and climate action on end of century (2071-2100) snow cover"){
  
  set(dat_lollipop, j = "country_plot", value = dat_lollipop[[lang]])
  
  dat_anno_repel <- merge(dat_anno_repel, dat_country_names, by = "country_fct")
  set(dat_anno_repel, j = "country_plot", value = dat_anno_repel[[lang]])
  
  country_anno_month <- dat_country_names[EN == "Croatia"][[lang]]
  
  gg <- 
    dat_lollipop[elev %in% c(500, 1500, 2500, 3500)] %>% 
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
    geom_text(data = data.frame(country_plot = factor(country_anno_month, levels = levels(dat_lollipop$country_plot))),
              x = 315, y = 1000, label = paste0("(~1 ", lbl_month,")"), colour = cols["anno_month"], size = 2.5)+
    geom_text(data = data.frame(country_plot = factor(country_anno_month, levels = levels(dat_lollipop$country_plot))),
              x = 315, y = 3000, label = paste0("30 ", lbl_days), colour = cols["anno_month"], size = 2.5)+
    geom_segment(data = data.frame(country_plot = factor(country_anno_month, levels = levels(dat_lollipop$country_plot))),
                 x = 300, xend = 330, y = 2000, yend = 2000, colour = cols["anno_month"],
                 arrow = arrow(ends = "both", type = "closed", length = unit(0.1, "in")))+
    
    
    # legend
    geom_label(data = dat_anno_repel,
               aes(x = rcp26_past, y = 4000), 
               hjust = 0, vjust = 1, size = 2.5, label.padding = unit(0.15, "lines"),
               label = lbl_recent)+
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
    facet_grid(country_plot ~ ., as.table = T, switch = "y", drop = T)+
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
    xlab(lbl_xlab)+
    ggtitle(lbl_title,
            lbl_subtitle)
  
  ggsave(gg, filename = paste0("fig/country/info-future-country_", lang, ".png"), width = 9, height = 6)
  
  gg
}



# english -----------------------------------------------------------------


dat_anno_repel <- cbind(dat_anno,
                        yy = 100 + c(1500 + 3.5*dy, 1500, 1500 - 3.5*dy),
                        xx = c(167, 120, 120) - 10,
                        colour = c("rcp26", "rcp85", "loss"),
                        label = c("... with 1.5-2°C warming",
                                  "reduction in future SCD with 4-5°C warming",
                                  "SCD saved with climate action"),
                        hjust = c(0, 0, 0))


f_plot()

## german ------------------------------------------------------------------


dat_anno_repel <- cbind(dat_anno,
                        yy = 100 + c(1500 + 3.5*dy, 1500, 1500 - 3.5*dy),
                        xx = c(171, 120, 120) - 10,
                        colour = c("rcp26", "rcp85", "loss"),
                        label = c("... mit 1.5-2°C Erwärmung",
                                  "Rückgang in der Zukunft mit 4-5°C Erwärmung",
                                  "Kann mit Klimaschutzmaßnahmen verhindert werden"),
                        hjust = c(0, 0, 0))


f_plot(lang = "DE",
       lbl_month = "Monat",
       lbl_days = "Tage",
       lbl_recent = "Gegenwart (2001-2020)",
       lbl_xlab = "Schneebedeckungsdauer [Tage]",
       lbl_title = "Schneebedeckungsdauer in den Alpen",
       lbl_subtitle = "Einfluss der globalen Erwärmung und der Klimaschutzmaßnahmen \nauf die Schneedecke am Ende dieses Jahrhunderts (2071-2100)")


## italian ------------------------------------------------------------------


dat_anno_repel <- cbind(dat_anno,
                        yy = 100 + c(1500 + 3.5*dy, 1500, 1500 - 3.5*dy),
                        xx = c(209.5, 120, 120) - 10,
                        colour = c("rcp26", "rcp85", "loss"),
                        label = c("... con ... 1.5-2°C",
                                  "Riduzione futura con un riscaldamento globale di 4-5°C",
                                  "Si può salvare con misure climatiche"),
                        hjust = c(0, 0, 0))


f_plot(lang = "IT",
       lbl_month = "mese",
       lbl_days = "giorni",
       lbl_recent = "Recente (2001-2020)",
       lbl_xlab = "Durata del manto nevoso [giorni]",
       lbl_title = "Durata del manto nevoso nelle Alpi",
       lbl_subtitle = "Impatto del riscaldamento globale e delle misure per l’adattamento \ne la mitigazione sulla copertura nevosa alla fine del secolo (2071-2100)")



## french ------------------------------------------------------------------


dat_anno_repel <- cbind(dat_anno,
                        yy = 100 + c(1500 + 3.5*dy, 1500, 1500 - 3.5*dy),
                        xx = c(215, 120, 120) - 10,
                        colour = c("rcp26", "rcp85", "loss"),
                        label = c("... avec ... 1.5-2°C",
                                  "Réduction future avec réchauffement climatique de 1.5-2°C",
                                  "Peut être sauvé grâce à actions d'atténuation"),
                        hjust = c(0, 0, 0))


f_plot(lang = "FR",
       lbl_month = "mois",
       lbl_days = "jours",
       lbl_recent = "Actuel (2001-2020)",
       lbl_xlab = "Durée de la couverture neigeuse [jours]",
       lbl_title = "Durée de la couverture neigeuse dans les Alpes",
       lbl_subtitle = "Effets du réchauffement climatique et de l'atténuation du changement\nclimatique sur le manteau neigeux à la fin du siècle (2071-2100)")


## spanish? ------------------------------------------------------------------




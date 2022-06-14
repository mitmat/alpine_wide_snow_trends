# info graphic future snow



library(data.table)
library(forcats)
library(ggplot2)
load("data/future-summary-elev-500m.rda")

setnames(dat_bc, "alt_f", "elev_f")
dat_bc[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]
dat_ds[, elev := tstrsplit(elev_f, ",") %>% sapply(readr::parse_number) %>% rowMeans]

dat_ens_mean <- dat_ds[elev > 200 & elev < 3600,
                       .(scd = round(mean(scd))),
                       .(elev_f, elev, experiment, period, fp)]


# dat_ens_mean <- dat_bc[elev > 200 & elev < 3600,
#                        .(scd = mean(snc)*365),
#                        .(elev_f, elev, experiment, period, fp)]

dat_ens_mean[, period_f := fct_recode(period,
                                      "2001\n-\n2020" = "2001-2020",
                                      "2041\n-\n2070" = "2041-2070",
                                      "2071\n-\n2100" = "2071-2100")]

dat_ens_mean[period != "2041-2070"] %>% 
  dcast(elev ~ experiment + fp, value.var = "scd") -> dat_lollipop

dat_lollipop[, elev_fct := fct_inorder(paste0(elev, " m"))]

dat_lollipop[, .(elev,
                 v1 = rcp85_future, 
                 v2 = rcp26_future - rcp85_future,
                 v3 = rcp26_past - rcp26_future)] %>% 
  melt(id.vars = "elev",
       measure.vars = paste0("v", 1:3)) %>% 
  .[,
    .(i_dot_grp = 1:round(value)),
    .(elev, variable)] -> dat_dp


dat_dp[, i_dot := 1:.N - 1, elev]
dat_dp[, xx := i_dot %% 10]
dat_dp[, yy := i_dot %/% 10]

# plot --------------------------------------------------------------------

x_rat <- 80
elev_plot <- c(500, 1500, 2500, 3500)

dat_dp[, xx_plot := elev + x_rat*xx - 9*x_rat/2]


dat_text <- dat_lollipop[elev %in% elev_plot,
                         .(elev, 
                           yy = ceiling(rcp26_past/10) + 1.5, 
                           past = round(rcp26_past),
                           loss = round(rcp26_past - rcp26_future),
                           climact = round(rcp26_future - rcp85_future))]
dat_text[, past_ch := sprintf("%6s", past)]
dat_text[, loss_ch := sprintf("%5s", loss)]
dat_text[, climact_ch := sprintf("%5s", climact)]




dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8), size = 0.2)+ # 1 month
  
  geom_point(shape = "\u2744", size = 2.5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = 1.5, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = 2, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = 2, colour = "#e6550d")+
  cowplot::theme_cowplot(line_size = 0.2)+
  theme(plot.background = element_rect(colour = "white", fill = "white"))+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 36.2 + 2), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Days with snow on ground in the Alps",
          "Impact of global warming and climate action for end of century (2071-2100) snow cover")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 2500, ymin = 28.5, ymax = 37.5, 
           colour = "white", fill = "white")+
  annotate("point", 100, 36, shape = "\u2744", size = 5, colour = "#9ecae1")+
  annotate("text", 150, 36, hjust = 0, vjust = 0.5, size = 3, 
           label = "Day with snow on ground, recent (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 33, shape = 4, size = 3, colour = "black")+
  annotate("text", 150, 33, hjust = 0, vjust = 0.5, size = 3, 
           label = "Future loss if global warming is 1.5-2°C (commited loss)", colour = "black")+
  
  annotate("point", 100, 30, shape = 1, size = 4, colour = "#e6550d")+
  annotate("text", 100, 30, label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  annotate("text", 150, 30, hjust = 0, vjust = 0.5, size = 3,
           label = "Extra loss if global warming is 4-5°C (can be saved with climate action)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 1000, xend = 1000, y = 18, yend = 15, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.075, "in")))+
  annotate("text", x = 1000, y = 16.5, hjust = -0.1, size = 3,
           label = "(~1 month)", colour = grey(0.8))+
  annotate("text", x = 1000, y = 16.5, hjust = 1.2, size = 3,
           label = "30 days", colour = grey(0.8))+
  
  # text
  geom_label(data = dat_text, 
            aes(elev - 270, yy, label = past_ch),
            hjust = 0.5, vjust = 0.5, colour = "#9ecae1", size = 3, 
            label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 270 - 60, yy),
             shape = "\u2744", size = 3, colour = "#9ecae1")+
  geom_label(data = dat_text, 
             aes(elev, yy, label = loss_ch),
             hjust = 0.5, vjust = 0.5, colour = "black", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 50, yy),
             shape = 4, size = 2, colour = "black")+
  geom_label(data = dat_text, 
             aes(elev + 250, yy, label = climact_ch),
             hjust = 0.5, vjust = 0.5, colour = "#e6550d", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev + 250 - 50, yy),
             shape = 1, size = 3, colour = "#e6550d")+
  geom_text(data = dat_text, 
            aes(elev + 250 - 50, yy, label = "?"),
            size = 3, colour = "#e6550d")

ggsave("fig/info-future_EN.png",
       width = 7, height = 4, units = "in")


ggsave("fig/info-future_EN_600dpi.png",
       width = 7, height = 4, units = "in", dpi = 600)



## german ------------------------------------------------------------------


dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8), size = 0.2)+ # 1 month
  
  geom_point(shape = "\u2744", size = 2.5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = 1.5, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = 2, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = 2, colour = "#e6550d")+
  cowplot::theme_cowplot(line_size = 0.2)+
  theme(plot.background = element_rect(colour = "white", fill = "white"))+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 36.2 + 2), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Tage mit Schneebedeckung in den Alpen",
          "Einfluss der globalen Erwärmung und der Klimaschutzmaßnahmen \nauf die Schneedecke am Ende dieses Jahrhunderts (2071-2100)")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 2500, ymin = 28.5, ymax = 37.5, 
           colour = "white", fill = "white")+
  annotate("point", 100, 36+1, shape = "\u2744", size = 5, colour = "#9ecae1")+
  annotate("text", 150, 36+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Tag mit Schneebedeckung, Gegenwart (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 33+1, shape = 4, size = 3, colour = "black")+
  annotate("text", 150, 33+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Zukünftiger Rückgang falls die globale Erwärmung 1.5-2°C beträgt", colour = "black")+
  
  annotate("point", 100, 30, shape = 1, size = 4, colour = "#e6550d")+
  annotate("text", 100, 30, label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  annotate("text", 150, 30, hjust = 0, vjust = 0.5, size = 3,
           label = "Zusätzlicher Rückgang falls die globale Erwärmung 4-5°C beträgt\n(kann durch Klimaschutzmaßnahmen verhindert werden)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 1000, xend = 1000, y = 18, yend = 15, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.075, "in")))+
  annotate("text", x = 1000, y = 16.5, hjust = -0.1, size = 3,
           label = "(~1 Monat)", colour = grey(0.8))+
  annotate("text", x = 1000, y = 16.5, hjust = 1.2, size = 3,
           label = "30 Tage", colour = grey(0.8))+
  
  # text
  geom_label(data = dat_text, 
             aes(elev - 270, yy, label = past_ch),
             hjust = 0.5, vjust = 0.5, colour = "#9ecae1", size = 3, 
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 270 - 60, yy),
             shape = "\u2744", size = 3, colour = "#9ecae1")+
  geom_label(data = dat_text, 
             aes(elev, yy, label = loss_ch),
             hjust = 0.5, vjust = 0.5, colour = "black", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 50, yy),
             shape = 4, size = 2, colour = "black")+
  geom_label(data = dat_text, 
             aes(elev + 250, yy, label = climact_ch),
             hjust = 0.5, vjust = 0.5, colour = "#e6550d", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev + 250 - 50, yy),
             shape = 1, size = 3, colour = "#e6550d")+
  geom_text(data = dat_text, 
            aes(elev + 250 - 50, yy, label = "?"),
            size = 3, colour = "#e6550d")

ggsave("fig/info-future_DE.png",
       width = 7, height = 4, units = "in")


ggsave("fig/info-future_DE_600dpi.png",
       width = 7, height = 4, units = "in", dpi = 600)




## italian -----------------------------------------------------------------


dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8), size = 0.2)+ # 1 month
  
  geom_point(shape = "\u2744", size = 2.5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = 1.5, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = 2, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = 2, colour = "#e6550d")+
  cowplot::theme_cowplot(line_size = 0.2)+
  theme(plot.background = element_rect(colour = "white", fill = "white"))+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 36.2 + 2), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Giorni con neve al suolo nelle Alpi",
          "Impatto del riscaldamento globale e delle misure per l’adattamento \ne la mitigazione sulla copertura nevosa alla fine del secolo (2071-2100)")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 2500, ymin = 28.5, ymax = 37.5, 
           colour = "white", fill = "white")+
  annotate("point", 100, 36+1, shape = "\u2744", size = 5, colour = "#9ecae1")+
  annotate("text", 150, 36+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Giorno con neve al suolo, recente (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 33+1, shape = 4, size = 3, colour = "black")+
  annotate("text", 150, 33+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Riduzione futura se il riscaldamento globale è di 1.5-2°C", colour = "black")+
  
  annotate("point", 100, 30, shape = 1, size = 4, colour = "#e6550d")+
  annotate("text", 100, 30, label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  annotate("text", 150, 30, hjust = 0, vjust = 0.5, size = 3,
           label = "Riduzione in più se il riscaldamento globale è di 4-5°C\n(si può salvare con misure climatiche)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 1000, xend = 1000, y = 18, yend = 15, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.075, "in")))+
  annotate("text", x = 1000, y = 16.5, hjust = -0.1, size = 3,
           label = "(~1 mese)", colour = grey(0.8))+
  annotate("text", x = 1000, y = 16.5, hjust = 1.2, size = 3,
           label = "30 giorni", colour = grey(0.8))+
  
  # text
  geom_label(data = dat_text, 
             aes(elev - 270, yy, label = past_ch),
             hjust = 0.5, vjust = 0.5, colour = "#9ecae1", size = 3, 
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 270 - 60, yy),
             shape = "\u2744", size = 3, colour = "#9ecae1")+
  geom_label(data = dat_text, 
             aes(elev, yy, label = loss_ch),
             hjust = 0.5, vjust = 0.5, colour = "black", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 50, yy),
             shape = 4, size = 2, colour = "black")+
  geom_label(data = dat_text, 
             aes(elev + 250, yy, label = climact_ch),
             hjust = 0.5, vjust = 0.5, colour = "#e6550d", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev + 250 - 50, yy),
             shape = 1, size = 3, colour = "#e6550d")+
  geom_text(data = dat_text, 
            aes(elev + 250 - 50, yy, label = "?"),
            size = 3, colour = "#e6550d")

ggsave("fig/info-future_IT.png",
       width = 7, height = 4, units = "in")


ggsave("fig/info-future_IT_600dpi.png",
       width = 7, height = 4, units = "in", dpi = 600)



## french -----------------------------------------------------------------


dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8), size = 0.2)+ # 1 month
  
  geom_point(shape = "\u2744", size = 2.5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = 1.5, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = 2, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = 2, colour = "#e6550d")+
  cowplot::theme_cowplot(line_size = 0.2)+
  theme(plot.background = element_rect(colour = "white", fill = "white"))+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 36.2 + 2), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Jours avec couverture neigeuse dans les Alpes",
          "Effets du réchauffement climatique et de l'atténuation du changement\nclimatique sur le manteau neigeux à la fin du siècle (2071-2100)")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 2500, ymin = 28.5, ymax = 37.5, 
           colour = "white", fill = "white")+
  annotate("point", 100, 36+1, shape = "\u2744", size = 5, colour = "#9ecae1")+
  annotate("text", 150, 36+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Jour avec de la neige au sol, actuel (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 33+1, shape = 4, size = 3, colour = "black")+
  annotate("text", 150, 33+1, hjust = 0, vjust = 0.5, size = 3, 
           label = "Réduction future si le réchauffement climatique est de 1.5-2°C", colour = "black")+
  
  annotate("point", 100, 30, shape = 1, size = 4, colour = "#e6550d")+
  annotate("text", 100, 30, label = "?", fontface = "plain", size = 4, colour = "#e6550d")+
  annotate("text", 150, 30, hjust = 0, vjust = 0.5, size = 3,
           label = "Réduction supplémentaire si le réchauffement climatique est de 4-5°C \n(peut être sauvé grâce à actions d'atténuation)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 1000, xend = 1000, y = 18, yend = 15, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.075, "in")))+
  annotate("text", x = 1000, y = 16.5, hjust = -0.1, size = 3,
           label = "(~1 mois)", colour = grey(0.8))+
  annotate("text", x = 1000, y = 16.5, hjust = 1.2, size = 3,
           label = "30 jours", colour = grey(0.8))+
  
  # text
  geom_label(data = dat_text, 
             aes(elev - 270, yy, label = past_ch),
             hjust = 0.5, vjust = 0.5, colour = "#9ecae1", size = 3, 
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 270 - 60, yy),
             shape = "\u2744", size = 3, colour = "#9ecae1")+
  geom_label(data = dat_text, 
             aes(elev, yy, label = loss_ch),
             hjust = 0.5, vjust = 0.5, colour = "black", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 50, yy),
             shape = 4, size = 2, colour = "black")+
  geom_label(data = dat_text, 
             aes(elev + 250, yy, label = climact_ch),
             hjust = 0.5, vjust = 0.5, colour = "#e6550d", size = 3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev + 250 - 50, yy),
             shape = 1, size = 3, colour = "#e6550d")+
  geom_text(data = dat_text, 
            aes(elev + 250 - 50, yy, label = "?"),
            size = 3, colour = "#e6550d")

ggsave("fig/info-future_FR.png",
       width = 7, height = 4, units = "in")


ggsave("fig/info-future_FR_600dpi.png",
       width = 7, height = 4, units = "in", dpi = 600)

## spanish? ----------------------------------------------------------------



# EOF ---------------------------------------------------------------------





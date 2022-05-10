# info graphic future snow

# mobile friendly layout (for dash)


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
dat_text[, past_ch := sprintf("%7s", past)]
dat_text[, loss_ch := sprintf("%6s", loss)]
dat_text[, climact_ch := sprintf("%6s", climact)]


mb_fact <- 11/14

dat_dp[elev %in% elev_plot] %>% 
  ggplot(aes(xx_plot, yy+0.5))+
  geom_hline(yintercept = 0:12*3, colour = grey(0.8), size = 0.2)+ # 1 month
  
  geom_point(shape = "\u2744", size = mb_fact*2.5, colour = "#9ecae1")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v3"] ,
             shape = 4, size = mb_fact*1.5, colour = "black")+
  geom_point(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
             shape = 1, size = mb_fact*2, colour = "#e6550d")+
  geom_text(data = dat_dp[elev %in% elev_plot & variable == "v2"] ,
            label = "?", fontface = "plain", size = mb_fact*2, colour = "#e6550d")+
  cowplot::theme_cowplot(line_size = 0.2, 11)+
  scale_x_continuous(NULL, limits = c(0, 4000), expand = c(0,0),
                     breaks = elev_plot, labels = paste0(elev_plot, " m"))+
  # scale_y_continuous(NULL, breaks = c(0,10,20,30), labels = c(0,10,20,30)*10)+
  scale_y_continuous(NULL, limits = c(0, 39.2 + 2), expand = c(0,0),
                     breaks = c(0,9,18,27,36), labels = c(0,9,18,27,36)*10)+
  ggtitle("Days with snow on ground in the Alps",
          "Impact of global warming and climate action\nfor end of century (2071-2100) snow cover")+
  
  #legend
  annotate("rect", xmin = 50, xmax = 2300, ymin = 33-1.5, ymax = 39+1.5, 
           colour = "white", fill = "white")+
  annotate("point", 100, 39, shape = "\u2744", size = mb_fact*5, colour = "#9ecae1")+
  annotate("text", 170, 39, hjust = 0, vjust = 0.5, size = mb_fact*3, 
           label = "Day with snow on ground, \nrecent (2001-2020)", colour = "#9ecae1")+
  
  annotate("point", 100, 36, shape = 4, size = mb_fact*3, colour = "black")+
  annotate("text", 170, 36, hjust = 0, vjust = 0.5, size = mb_fact*3, 
           label = "Future loss if global warming is 1.5-2°C \n(commited loss)", colour = "black")+
  
  annotate("point", 100, 33, shape = 1, size = mb_fact*4, colour = "#e6550d")+
  annotate("text", 100, 33, label = "?", fontface = "plain", size = mb_fact*4, colour = "#e6550d")+
  annotate("text", 170, 33, hjust = 0, vjust = 0.5, size = mb_fact*3,
           label = "Extra loss if global warming is 4-5°C \n(can be saved with climate action)",
           colour = "#e6550d")+
  
  # grid stuff
  annotate("segment", x = 600, xend = 600, y = 21, yend = 18, colour = grey(0.8),
           arrow = arrow(ends = "both", type = "closed", length = unit(0.075, "in")))+
  annotate("text", x = 600, y = 19.5, hjust = -0.1, size = mb_fact*3,
           label = "(~1 month)", colour = grey(0.8))+
  annotate("text", x = 600, y = 19.5, hjust = 1.2, size = mb_fact*3,
           label = "30 days", colour = grey(0.8))+
  
  # text
  geom_label(data = dat_text, 
            aes(elev - 270, yy + 3, label = past_ch),
            hjust = 0.5, vjust = 0.5, colour = "#9ecae1", size = mb_fact*3, 
            label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 270 - 90, yy + 3),
             shape = "\u2744", size = mb_fact*3, colour = "#9ecae1")+
  geom_label(data = dat_text, 
             aes(elev, yy + 1.5, label = loss_ch),
             hjust = 0.5, vjust = 0.5, colour = "black", size = mb_fact*3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev - 60, yy + 1.5),
             shape = 4, size = mb_fact*2, colour = "black")+
  geom_label(data = dat_text, 
             aes(elev + 250, yy, label = climact_ch),
             hjust = 0.5, vjust = 0.5, colour = "#e6550d", size = mb_fact*3,
             label.padding = unit(0.15, "lines"), label.size = 0.12)+
  geom_point(data = dat_text, 
             aes(elev + 250 - 80, yy),
             shape = 1, size = mb_fact*3, colour = "#e6550d")+
  geom_text(data = dat_text, 
            aes(elev + 250 - 80, yy, label = "?"),
            size = mb_fact*3, colour = "#e6550d")

ggsave("fig/info-future_EN_mb.png",
       width = 3.75, height = 4.8, units = "in")


´# ggsave("fig/info-future_EN_600dpi.png",
#        width = 7, height = 4, units = "in", dpi = 600)

# EOF ---------------------------------------------------------------------





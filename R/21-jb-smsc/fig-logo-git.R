# library(data.table)
library(magrittr)
library(ggplot2)

dat_mountain <- readRDS("data/mountain-background.rds")
dat_mountain_sub <- dat_mountain[200: 400, ]

dat_mountain_sub %>% summary

dat_mountain_sub %>% 
  ggplot(aes(ii, elev_rollmean))+
  geom_line()+
  geom_point(aes(x = 300, y = 1000), shape = "\u2744", size = 22, colour = "#9ecae1")+
  ylim(0, 3500)+
  theme_void()+
  # theme(plot.background = element_rect(fill = "#9ecae1"))
  theme(plot.background = element_rect(fill = "white"))

# dat_mountain_sub %>% 
#   ggplot(aes(ii, elev_rollmean))+
#   geom_point(aes(x = 300, y = 2000), shape = "\u2744", size = 50, colour = "white")+
#   geom_line(linewidth = 1)+
#   ylim(0, 4000)+
#   theme_void()+
#   theme(plot.background = element_rect(fill = "#9ecae1"))


ggsave("fig/logo-jb-smsc.png", width = 600, height = 600, units = "px")

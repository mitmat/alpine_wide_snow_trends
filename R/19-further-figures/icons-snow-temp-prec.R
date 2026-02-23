library(ggplot2)

# icons only --------------------------------------------------------------


gg_icon_snow <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("snowflake", x = 200, y = 700, size = 50)

gg_icon_tmean <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("thermometer", x = 200, y = 700, size = 50)

gg_icon_prec <- ggplot()+
  theme_void()+
  emojifont::geom_emoji("cloud_with_rain", x = 200, y = 700, size = 50)

ggsave("fig/icon-snow.png", gg_icon_snow, width = 3, height = 3)
ggsave("fig/icon-tmean.png", gg_icon_tmean, width = 3, height = 3)
ggsave("fig/icon-prec.png", gg_icon_prec, width = 3, height = 3)

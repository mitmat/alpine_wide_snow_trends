library(dplyr)
library(sf)
library(ggplot2)


sf_alps <- read_sf("/mnt/CEPH_BASEDATA/GIS/ALPS/BOUNDARIES/NUTS2_Alpen_2006_utm.shp")
sf_alps

# plot(sf_alps)
# sf_alps$STAT_LEVL_ %>% table

# sf_alps %>% 
#   filter(startsWith(NUTS_ID, "IT")) %>% 
#   plot()

sf_alps_it <- sf_alps %>% 
  filter(startsWith(NUTS_ID, "IT"))

ggplot()+
  borders(colour = "grey50")+
  geom_sf(data = sf_alps_it, fill = NA, colour = "grey50")+
  coord_sf(xlim = c(4,17), ylim = c(43,49))+
  theme_bw()+
  xlab(NULL)+ylab(NULL)

library(rgeos)
library(rgdal)
library(maptools)
gpclibPermit()
library(ggplot2)
library(viridisLite)

tract <- readOGR("H:/irid_time_trends_HES_2022/lad_ug/Local_Authority_Districts_December_2017_Ultra_Generalised_Clipped_Boundaries_in_United_Kingdom_WGS84.shp")

y <- fortify(tract, region = 'lad17cd')
y <- y[substr(y$id, 0, 1) == 'E',]

cols <- magma(50)
cols <- colorRampPalette(c('white', 'red'))(50)

png('admission_rate_map.png', height = 8, width = 8, units = 'in', res = 300)

ggplot() + 
  geom_map(data = la_rate, aes(map_id=LAD10CD, fill = rate), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_gradientn(colours = cols, na.value = 'blue') +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

dev.off()

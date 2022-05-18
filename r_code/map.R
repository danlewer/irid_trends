library(rgeos)
library(rgdal)
library(maptools)
gpclibPermit()
library(ggplot2)

la_rate <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/standardised_rate_by_LA_18may2022.csv'))
la_rate <- la_rate[is.na(la_rate$period),]

# from https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2017-boundaries-gb-buc/about
tract <- readOGR('Local_Authority_Districts__December_2017__Boundaries_GB_BUC.shp')

y <- fortify(tract, region = 'LAD17CD')
y <- y[substr(y$id, 0, 1) == 'E',]

cols <- colorRampPalette(c('white', "#E41A1C", 'black'))(50)
cols <- cols[1:45]

la_rate <- merge(data.frame(LAD10CD = unique(y$id)), la_rate, all.x = T)

png('admission_rate_map.png', height = 7, width = 8, units = 'in', res = 300)

ggplot() + 
  geom_map(data = la_rate, aes(map_id=LAD10CD, fill = sr), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_gradientn(colours = cols, na.value = 'grey70') +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

dev.off()

head(la_rate[order(la_rate$sr, decreasing = T),], 10)

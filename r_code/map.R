library(rgeos)
library(rgdal)
library(maptools)
gpclibPermit()
library(ggplot2)
library(extrafont)
loadfonts(device = "win")

la_rate <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/standardised_rate_by_LA_25may2022.csv'))
la_rate <- la_rate[is.na(la_rate$period),]

#  ===
#  map
#  ---

# from https://geoportal.statistics.gov.uk/datasets/ons::local-authority-districts-december-2017-boundaries-gb-buc/about
tract <- readOGR('Local_Authority_Districts__December_2017__Boundaries_GB_BUC.shp')

y <- fortify(tract, region = 'LAD17CD')
y <- y[substr(y$id, 0, 1) == 'E',]

cols <- colorRampPalette(c('white', "#E41A1C", 'black'))(50)
cols <- cols[1:40]

la_rate <- merge(data.frame(LAD10CD = unique(y$id)), la_rate, all.x = T)

emf('admission_rate_map.emf', height = 7, width = 8, family = 'Georgia')

ggplot() + 
  geom_map(data = la_rate, aes(map_id=LAD10CD, fill = sr), map=y) +
  geom_path(aes(x = long, y = lat, group = group), data = y, color = 'black', lwd = 0.2) + 
  scale_fill_gradientn(colours = cols, na.value = 'grey70') +
  theme_bw() + 
  theme(axis.ticks = element_blank(), axis.text = element_blank(), axis.text.y = element_blank(), panel.grid.minor = element_blank(), panel.grid.major = element_blank(), axis.title.x = element_blank(), axis.title.y = element_blank(), panel.border = element_blank(), legend.title = element_blank())

dev.off()

setDT(la_rate)

head(la_rate[order(sr, decreasing = T), -c('LAD10CD', 'RESLADST', 'period', 'X50.')], 10)

> head(la_rate[order(sr, decreasing = T), -c('LAD10CD', 'RESLADST', 'period', 'X50.')], 10)
#                     GOR_NAME                   LA09_NAME       sr    X2.5.   X97.5.
# 1:               North East                  Hartlepool 907.0401 847.8065 970.1319
# 2:               North West                   Blackpool 766.4197 722.5797 809.4041
# 3:            East Midlands                   Mansfield 759.0841 708.8277 811.3660
# 4:               North East               Middlesbrough 578.9489 544.7181 616.1633
# 5: Yorkshire and The Humber Kingston upon Hull, City of 557.4561 531.6575 582.7277
# 6:               North West                   Liverpool 491.8319 473.7993 510.8267
# 7: Yorkshire and The Humber                   Doncaster 463.3658 440.8509 485.4299
# 8:            East Midlands                Chesterfield 442.5087 405.1735 480.5413
# 9:            East Midlands                     Lincoln 442.0698 404.0684 482.2432
# 10: Yorkshire and The Humber                    Barnsley 430.0150 405.3242 457.4570

tail(la_rate[order(sr, decreasing = T), -c('LAD10CD', 'RESLADST', 'period', 'X50.')][!is.na(sr)], 10)

#           GOR_NAME            LA09_NAME       sr     X2.5.   X97.5.
# 1:      South East    South Oxfordshire 28.37864 18.999834 38.29632
# 2:          London            Redbridge 28.00547 22.272514 34.23425
# 3:      South East Reigate and Banstead 27.73296 18.954504 37.42737
# 4: East of England         Three Rivers 25.91035 14.301470 38.36472
# 5: East of England        Epping Forest 24.97403 16.060079 34.01319
# 6:      South East      Epsom and Ewell 24.89470  7.417509 45.13316
# 7:          London               Harrow 24.70610 18.183995 31.70259
# 8:          London               Merton 22.66736 17.183979 28.91213
# 9:          London              Enfield 22.20139 17.441375 27.88017
# 10: East of England            Braintree 20.28892 11.823847 29.97468

#  =======
#  barplot
#  -------

setDT(la_rate)
key <- data.table(GOR_NAME = c('East of England', 'London', 'South East', 'West Midlands', 'East Midlands', 'South West', 'North West', 'Yorkshire and The Humber', 'North East'))
key[, cl := brewer.pal(.N, 'Spectral')]
key[, cl := colorRampPalette(c('white', 'lightblue', 'orange', 'red', 'black'))(.N)]

la_rate <- key[la_rate, on = 'GOR_NAME']
la_rate <- la_rate[order(sr)]
la_rate[, y := .I]
ys <- seq(110, 184, length.out = nrow(key) + 1)

emf('la_barplot_27may2022.emf', height = 12, width = 8, family = 'Franklin Gothic Book')

par(mar = c(4, 3, 0, 0))
plot(1, type = 'n', xlim = c(0, 1000), ylim = c(0, 294), axes = F, xlab = NA, ylab = NA)
with(la_rate, {
  rect(0, y-1, sr, y, col = cl, lwd = 0.5)
  arrows(X2.5., y-0.5, X97.5., y-0.5, length = 0, code = 3, angle = 90, lwd = 0.5)
})
axis(1, 0:5 * 200, pos = 0)
rect(600, ys[-length(ys)], 640, ys[-1], col = key$cl)
text(650, ys[-length(ys)] + diff(ys)/2, key$GOR_NAME, adj = 0)
title(xlab = 'Opiate injection-related infections per million,\nStandardised by age and year')
title(ylab = 'Local authority districts', line = 1)

dev.off()

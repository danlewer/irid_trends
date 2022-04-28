library(RColorBrewer)
library(viridisLite)


yrAge <- rbind(0, yrAge)
yrAgeCum <- apply(yrAge, 2, cumsum)
xleft <- matrix(rep(1998:2021, 5), ncol = length(1998:2021), byrow = T)

png('count_and_boxplot.png', height = 14, width = 9, units = 'in', res = 300)

par(mar = c(4, 5, 1, 10), xpd = NA, mfrow = c(2, 1))

cols <- viridis(5)
ys <- seq(2500, 4500, length.out = 6)

plot(1, type = 'n', xlim = c(1998, 2022), ylim = c(0, 7500), axes = F, ylab = 'Count of admissions', xlab = NA)
rect(xleft = xleft, ybottom = yrAgeCum[-6,], xright = xleft + 1, ytop = yrAgeCum[-1,], col = cols)
axis(2, 0:15 * 500, pos = 1998, las = 2)
axis(1, 1998:2022, labels = F, pos = 0)
segments(2022, 0, y1 = 7500)
segments(1998, 7500, x1 = 2022)
text(1998:2021 + 0.5, -250, labels = 1998:2021, srt = 60, adj = 1)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + 300, 'Age at\nadmission', adj = 0)

gap <- 0.3
ys <- seq(30, 50, length.out = 5)

par(mar = c(5, 5, 1, 10), xpd = NA)
plot(1, type = 'n', xlim = c(1998, 2022), ylim = c(20, 60), axes = F, xlab = 'Year', ylab = 'Age')
arrows(x0 = 1998:2021 + 0.5, y0 = age_quantiles[,1], y1 = age_quantiles[,5], length = 0.05, code = 3, angle = 90)
rect( 
  xleft = 1998:2021 + 0.5 - gap, 
  ybottom = age_quantiles[,2], 
  xright = 1998:2021 + 0.5 + gap,
  ytop = age_quantiles[,4],
  col = cols[4])
segments(x0 = 1998:2021 + 0.5 - gap, y0 = age_quantiles[,3], x1 = 1998:2021 + 0.5 + gap)
axis(1, 1998:2022, labels = F, pos = 20)
text(1998:2021 + 0.5, 18, labels = 1998:2021, srt = 60, adj = 1)
axis(2, seq(20, 60, 10), pos = 1998, las = 2)
segments(2022, 20, y1 = 60)
segments(1998, 60, x1 = 2022)
arrows(2023.5, y0 = ys[1], y1 = ys[5], length = 0.05, code = 3, angle = 90)
rect(2023.5 - gap, ys[2], 2023.5 + gap, ys[4], col = cols[4])
segments(x0 = 2023.5 - gap, y0 = ys[3], x1 = 2023.5 + gap)
text(2023.5 + gap + 0.5, ys, c(0.05, 0.25, 0.5, 0.75, 0.95), adj = 0)
text(2023.5, max(ys) + 3, 'Quantile\nof age', adj = 0)

dev.off()

#  =======
#  COVID19
#  -------

lockdown <- which(names(covid19) == 'Mar 2020')

png('covid19.png', height = 7, width = 9, units = 'in', res = 300)

par(xpd = NA, mar = c(6, 5, 1, 1))
plot(1, type = 'n', xlim = c(0, 50), ylim = c(0, 800), ylab = NA, xlab = NA, axes = F)
points(seq_along(covid19), covid19, pch = 19)
lines(seq_along(covid19), covid19)
segments(lockdown, 0, y1 = 800, lty = 2)
axis(1, seq_along(covid19), labels = F, pos = 0, tck = -0.01)
axis(1, seq_along(covid19)[grepl('Jan', names(covid19))], pos = 0, labels = F, tck = -0.02)
text(seq_along(covid19)[grepl('Jan', names(covid19))], -30, names(covid19)[grepl('Jan', names(covid19))], srt = 60, adj = 1)
axis(2, 0:8 * 100, pos = 1, las = 2)
segments(1, 800, x1 = length(covid19))
segments(length(covid19), 0, y1 = 800)
title(ylab = 'Number of admissions for injecting-related infections\nin England per month', line = 1.5)
title(xlab = 'Date of admission', line = 4)
text(lockdown - 1, 300, 'Lockdown', srt = 90)

dev.off()

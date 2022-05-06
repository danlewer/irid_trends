library(data.table)
library(RColorBrewer)
library(viridisLite)

yax <- function(x, tickabove = F, ntick = 5) { # create axis tick-points: https://gist.github.com/danlewer/8ec82abcbeb9e5b1ad08dab02a0091a2
  l <- c(c(1, 2, 4, 5, 25) %o% 10^(0:8))
  d <- l[which.min(abs(x/ntick - l))]
  d <- 0:(ntick+1) * d
  i <- findInterval(x, d)
  if (tickabove) {i <- i + 1}
  d[seq_len(i)]
}

#  ::::::::::::::::
#  Histogram of age
#  ................

age_hist <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_histogram_6may2022.csv'))
age_qs <- quantile(rep(age_hist$STARTAGE, age_hist$n), probs = c(0.25, 0.5, 0.75))

png('age_histogram.png', height = 7, width = 9, units = 'in', res = 300)

par(mar = c(5, 5, 2, 1), xpd = NA)
plot(1, type = 'n', xlim = c(15, 65), ylim = c(0, 5500), axes = F, xlab = NA, ylab = NA)
axis(1, 15:65, labels = F, pos = 0, tck = -0.007)
axis(1, seq(15, 65, 5), labels = T, pos = 0, tck = -0.02)
axis(2, seq(0, 5500, 500), pos = 15, las = 2)
segments(15, 5500, x1 = 65)
segments(65, 0, y1 = 5500)
segments(age_qs, 0, y1 = 5500, lty = 2)
text(age_qs, 5800, paste0(c('Lower Q.', 'Median', 'Upper Q.'), '\n', age_qs))
rect(15:64, 0, 16:65, age_hist$n, col = viridis(6)[5])
title(xlab = 'Age at admission', line = 2)
title(ylab = 'Number of admissions, 1998-2021')

dev.off()

#  ======================================================
#  Compare DRDs with injecting-related infections by year
#  ------------------------------------------------------

drds <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/drug_poisoning_registrations.csv'))
setDT(drds)
admissions <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group.csv'))
setDT(admissions)

drd_summary <- drds[Substance == '01 All drug poisonings' & Usual.residence.name == 'England' & Year.of.death.registration >= 1998, .(drds = sum(Deaths)), Year.of.death.registration]
#drd_summary <- drds[Substance == '02 Any opiate (includes unspecified opiates, excludes paracetamol compounds)' & Usual.residence.name == 'England' & Year.of.death.registration >= 1998, .(drds = sum(Deaths)), Year.of.death.registration]
setnames(drd_summary, 'Year.of.death.registration', 'year')
admission_summary <- admissions[, .(admissions = sum(admissions)), year]
admissions_vs_drds <- drd_summary[admission_summary, on = 'year'][order(year)]

ref_year <- admissions_vs_drds[year == 2005]
admissions_vs_drds[, admissions_index := admissions / ref_year[, admissions]]
admissions_vs_drds[, drds_index := drds / ref_year[, drds]]

ymax <- 1.8
yx1 <- yax(ymax * ref_year[, admissions], ntick = 6)
yx2 <- yax(ymax * ref_year[, drds], ntick = 8)
cols <- brewer.pal(3, 'Set1')

png('admissions_vs_drds.png', height = 8, width = 8, units = 'in', res = 300)

par(xpd = NA, mar = c(6, 6, 1, 6))
plot(1, type = 'n', ylim = c(0, ymax), xlim = c(1997, 2022), axes = F, xlab = NA, ylab = NA)

with(admissions_vs_drds[year >= 1998], {
  lines(year, admissions_index, type = 'b', pch = 19, col = cols[1])
  lines(year, drds_index, type = 'b', pch = 19, col = cols[2])
})

axis(1, 1997:2022, labels = F, pos = 0)
text(1998:2021, -0.07, labels = 1998:2021, srt = 60, adj = 1)
axis(2, yx1/ref_year[, admissions], yx1, las = 2, pos = 1997, col = cols[1], col.axis = cols[1])
axis(4, yx2/ref_year[, drds], yx2, las = 2, pos = 2022, col = cols[2], col.axis = cols[2])

segments(1997, 0, y1 = ymax, col = cols[1])
segments(1997, ymax, x1 = 2022)
segments(2022, 0, y1 = ymax, col = cols[2])

mtext('Number of hospital admissions for treatment of\nopiate injecting-related bacterial infections in England', side = 2, line = 4, col = cols[1])
mtext('Number of deaths due to drug poisoning\nregistered in England', side = 4, line = 4, col = cols[2])
title(xlab = 'Calendar year', line = 3)

dev.off()

#  =============================
#  Age-stratified counts by year
#  -----------------------------

yrAge <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group.csv'))
yrAge <-with(yrAge, xtabs(admissions ~ age_group + year))
yrAge <- rbind(0, yrAge)
yrAgeCum <- apply(yrAge, 2, cumsum)
xleft <- matrix(rep(1998:2021, 5), ncol = length(1998:2021), byrow = T)

age_quantiles <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_quantiles.csv'))
age_quantiles <- age_quantiles[age_quantiles$region == 'England',]
age_quantiles <- age_quantiles[, c(3:7)]

cols <- viridis(5)

png('count_and_boxplot.png', height = 14, width = 9, units = 'in', res = 300)

par(mar = c(4, 5, 1, 10), xpd = NA, mfrow = c(2, 1))

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

irid_monthly <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/monthly_from_jan2018.csv'))
other_monthly <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/other_causes_monthly_admissions.csv'))
setDT(irid_monthly); setDT(other_monthly)
irid_monthly <- other_monthly[irid_monthly, on = 'admiMonth']
month_names <- c(outer(month.abb, paste0(' ', 2018:2021), paste0))
irid_monthly[, admiMonth := factor(admiMonth, month_names)]
irid_monthly <- irid_monthly[order(admiMonth)]
index <- irid_monthly[, lapply(.SD, function (x) x / x[1] * irid_monthly$N[1]), .SDcols = names(irid_monthly)[-1]]

lockdown <- which(irid_monthly$admiMonth == 'Mar 2020')

xs <- seq_along(irid_monthly$admiMonth)
linesb <- function (x, y, ...) {
  points(x, y, pch = 19, ...)
  lines(x, y, ...)
}
cols <- brewer.pal(4, 'Set1')

yx2 <- seq(0, 1.4, 0.2)

png('covid19.png', height = 7, width = 11, units = 'in', res = 300)

par(xpd = NA, mar = c(6, 5, 2, 15))
plot(1, type = 'n', xlim = c(0, 48), ylim = c(0, 800), ylab = NA, xlab = NA, axes = F)
rect(lockdown, 0, lockdown + 4, 800, col = 'grey87', border = NA)
text(lockdown, 840, 'First\nLockdown', adj = 0)
axis(1, xs, labels = F, pos = 0, tck = -0.01)
axis(1, xs[grepl('Jan', irid_monthly$admiMonth)], pos = 0, labels = F, tck = -0.02)
text(xs[grepl('Jan', irid_monthly$admiMonth)], -30, irid_monthly$admiMonth[grepl('Jan', irid_monthly$admiMonth)], srt = 60, adj = 1)
axis(2, 0:8 * 100, pos = 1, las = 2)
axis(4, yx2 * irid_monthly$N[1], yx2, pos = 48, las = 2)
segments(1, 800, x1 = 48)
segments(48, 0, y1 = 800)
with(index, {
  linesb(xs, N, lwd = 1.5)
  linesb(xs, all_cause, col = cols[1], lty = 3, cex = 0.6)
  linesb(xs, asthma, col = cols[2], lty = 3, cex = 0.6)
  linesb(xs, appendix, col = cols[3], lty = 3, cex = 0.6)
  linesb(xs, diabetes, col = cols[4], lty = 3, cex = 0.6)
})
mtext('Count of admissions due to\ninjecting-related infections', side = 2, line = 2.5)
mtext('Ratio of admissions\nvs. Jan 2018', side = 4, line = 3)
title(xlab = 'Date of admission', line = 4)
ys <- seq(600, 800, length.out = 5)
segments(54, ys, x1 = 57, col = c(cols, 'black'), lty = c(3, 3, 3, 3, 1), lwd = c(1, 1, 1, 1, 1.5))
points(rep(55.5, 5), ys, col = c(cols, 'black'), cex = c(0.6, 0.6, 0.6, 0.6, 1), pch = 19)
text(57.5, ys, c('All cause', 'Asthma', 'Appendix', 'Diabetes', 'Injecting-related infections'), adj = 0)

dev.off()

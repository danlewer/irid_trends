library(data.table)
library(RColorBrewer)
library(viridisLite)
library(devEMF)
library(extrafont)
loadfonts(device = "win")

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

age_hist <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_histogram_25may2022.csv'))
age_qs <- quantile(rep(age_hist$STARTAGE, age_hist$n), probs = c(0.25, 0.5, 0.75))

emf('age_histogram.emf', height = 5, width = 5, family = 'Georgia')

par(mar = c(5, 5, 6, 1), xpd = NA)
plot(1, type = 'n', xlim = c(15, 65), ylim = c(0, 5500), axes = F, xlab = NA, ylab = NA)
axis(1, 15:65, labels = F, pos = 0, tck = -0.015)
axis(1, seq(15, 65, 5), labels = T, pos = 0, tck = -0.04)
axis(2, seq(0, 5500, 500), pos = 15, las = 2)
segments(15, 5500, x1 = 65)
segments(65, 0, y1 = 5500)
segments(age_qs, 0, y1 = 5500, lty = 2)
text(age_qs, 5700, age_qs)
rect(15:64, 0, 16:65, age_hist$n, col = viridis(6)[5])
title(xlab = 'Age at admission', line = 2)
title(ylab = 'Number of admissions, 2002-2021')

dev.off()

#  ======================================================
#  Compare DRDs with injecting-related infections by year
#  ------------------------------------------------------

drds <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/drug_poisoning_registrations.csv'))
setDT(drds)
admissions <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_25may2022.csv'))
setDT(admissions)

drd_summary <- drds[Substance == '01 All drug poisonings' & Usual.residence.name == 'England' & Year.of.death.registration >= 1998, .(drds = sum(Deaths)), Year.of.death.registration]
#drd_summary <- drds[Substance == '02 Any opiate (includes unspecified opiates, excludes paracetamol compounds)' & Usual.residence.name == 'England' & Year.of.death.registration >= 1998, .(drds = sum(Deaths)), Year.of.death.registration]
setnames(drd_summary, 'Year.of.death.registration', 'year')
admission_summary <- admissions[, .(admissions = sum(admissions)), year]
admissions_vs_drds <- drd_summary[admission_summary, on = 'year'][order(year)]

ref_year <- admissions_vs_drds[year == 2002]
admissions_vs_drds[, admissions_index := admissions / ref_year[, admissions]]
admissions_vs_drds[, drds_index := drds / ref_year[, drds]]
admissions_vs_drds <- admissions_vs_drds[year >= 2002]

ymax <- 1.9
yx1 <- yax(ymax * ref_year[, admissions], ntick = 6)
yx2 <- yax(ymax * ref_year[, drds], ntick = 8)
cols <- brewer.pal(3, 'Set1')

# emf('admissions_vs_drds.emf', height = 5, width = 6, family = 'Georgia')
png('admissions_vs_drds.png', height = 5, width = 6, units = 'in', res = 300, family = 'Georgia')

par(xpd = NA, mar = c(6, 6, 1, 6))
plot(1, type = 'n', ylim = c(0, ymax), xlim = c(2001, 2022), axes = F, xlab = NA, ylab = NA)

with(admissions_vs_drds[year >= 1998], {
  lines(year, admissions_index, col = cols[1])
  points(year, admissions_index, pch = 19, col = cols[1])
  lines(year, drds_index, pch = 19, col = cols[2])
  points(year, drds_index, pch = 19, col = cols[2])
})

axis(1, 2001:2022, labels = F, pos = 0)
text(2002 + 0:9 * 2, -0.1, labels = 2002 + 0:9 * 2, srt = 60, adj = 1)
axis(2, yx1/ref_year[, admissions], yx1, las = 2, pos = 2001, col = cols[1], col.axis = cols[1])
axis(4, yx2/ref_year[, drds], yx2, las = 2, pos = 2022, col = cols[2], col.axis = cols[2])

segments(2001, 0, y1 = ymax, col = cols[1])
segments(2001, ymax, x1 = 2022)
segments(2022, 0, y1 = ymax, col = cols[2])

mtext('Hospital admissions for opiate injecting\n-related bacterial infections', side = 2, line = 4, col = cols[1])
mtext('Deaths due to drug poisoning', side = 4, line = 4, col = cols[2])
title(xlab = 'Calendar year', line = 3)

dev.off()

#  =============================
#  Age-stratified counts by year
#  -----------------------------

yrAge <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_25may2022.csv'))

# change from 2011 - 2021 by age group
decade <- xtabs(admissions ~ age_group + year, yrAge[yrAge$year %in% c(2011, 2021),])
(decade[,2] - decade[,1]) / decade[,1] * 100

yrAge <- yrAge[yrAge$year >= 2002 & yrAge$year <= 2021,]
yrAge <- with(yrAge, xtabs(admissions ~ age_group + year))
yrAge <- rbind(0, yrAge)
yrAgeCum <- apply(yrAge, 2, cumsum)
xleft <- matrix(rep(2002:2021, 5), ncol = length(2002:2021), byrow = T)

age_quantiles <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_quantiles_25may2022.csv'))
age_quantiles <- age_quantiles[age_quantiles$region == 'England',]
age_quantiles <- age_quantiles[, c(3:7)]

cols <- viridis(5)

#emf('count_and_boxplot.emf', height = 14, width = 9, family = 'Georgia')
png('count_and_boxplot.png', height = 14, width = 9, units = 'in', res = 300, family = 'Georgia')

par(mar = c(4, 5, 1, 10), xpd = NA, mfrow = c(2, 1))

ys <- seq(2500, 4500, length.out = 6)

plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 7500), axes = F, ylab = 'Count of admissions', xlab = NA)
rect(xleft = xleft, ybottom = yrAgeCum[-6,], xright = xleft + 1, ytop = yrAgeCum[-1,], col = cols)
axis(2, 0:15 * 500, pos = 2002, las = 2)
axis(1, 2002:2022, labels = F, pos = 0)
segments(2022, 0, y1 = 7500)
segments(2002, 7500, x1 = 2022)
text(2002:2021 + 0.5, -250, labels = 2002:2021, srt = 60, adj = 1)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + 300, 'Age at\nadmission', adj = 0)

gap <- 0.3
ys <- seq(30, 50, length.out = 5)

par(mar = c(5, 5, 1, 10), xpd = NA)
plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(20, 60), axes = F, xlab = 'Year', ylab = 'Age at admission')
arrows(x0 = 2002:2021 + 0.5, y0 = age_quantiles[,1], y1 = age_quantiles[,5], length = 0.05, code = 3, angle = 90)
rect( 
  xleft = 2002:2021 + 0.5 - gap, 
  ybottom = age_quantiles[,2], 
  xright = 2002:2021 + 0.5 + gap,
  ytop = age_quantiles[,4],
  col = cols[4])
segments(x0 = 2002:2021 + 0.5 - gap, y0 = age_quantiles[,3], x1 = 2002:2021 + 0.5 + gap)
axis(1, 2002:2022, labels = F, pos = 20)
text(2002:2021 + 0.5, 18, labels = 2002:2021, srt = 60, adj = 1)
axis(2, seq(20, 60, 10), pos = 2002, las = 2)
segments(2022, 20, y1 = 60)
segments(2002, 60, x1 = 2022)
arrows(2023.5, y0 = ys[1], y1 = ys[5], length = 0.05, code = 3, angle = 90)
rect(2023.5 - gap, ys[2], 2023.5 + gap, ys[4], col = cols[4])
segments(x0 = 2023.5 - gap, y0 = ys[3], x1 = 2023.5 + gap)
text(2023.5 + gap + 0.5, ys, c(0.05, 0.25, 0.5, 0.75, 0.95), adj = 0)
text(2023.5, max(ys) + 3, 'Quantile\nof age', adj = 0)

dev.off()

#  =======
#  COVID19
#  -------

irid_monthly <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/monthly_from_jan2018_25may2022.csv'))
other_monthly <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/monthly_trends_other_causes_25may2022.csv'))
setDT(irid_monthly); setDT(other_monthly)
names(other_monthly)[1] <- 'admiMonth'
irid_monthly <- other_monthly[irid_monthly, on = 'admiMonth']
month_names <- c(outer(month.abb, paste0(' ', 2018:2021), paste0))
irid_monthly[, admiMonth := factor(admiMonth, month_names)]
irid_monthly <- irid_monthly[order(admiMonth)]

index1 <- irid_monthly[, -1][, lapply(.SD, function (x) x / max(x, na.rm = T))]
index2 <- irid_monthly[, -1][, lapply(.SD, function (x) x / x[1])]
index3 <- irid_monthly[, -1][, lapply(.SD, function (x) x / x[1] * irid_monthly$N[1])]

lockdown <- which(irid_monthly$admiMonth == 'Mar 2020')
xs <- seq_along(irid_monthly$admiMonth)
linesb <- function (x, y, ...) {
  points(x, y, pch = 19, ...)
  lines(x, y, ...)
}
yx2 <- seq(0, 1.6, 0.2)
comps <- c('N', 'drugs', 'alcohol', 'asthma', 'intestinal_infections', 'resp_infections', 'all_cause')
label <- c('Injecting-related infections', 'Illicit drugs', 'Alcohol', 'Asthma', 'Intestinal Infections', 'Respiratory infections (ex. COVID-19)', 'All cause')
cols <- c('black', brewer.pal(length(comps) - 1, 'Paired'))

emf('covid19.emf', height = 7, width = 14, family = 'Georgia')

par(xpd = NA, mar = c(6, 5, 2, 22))
plot(1, type = 'n', xlim = c(0, 48), ylim = c(0, 900), ylab = NA, xlab = NA, axes = F)
segments(irid_monthly[, grep('Jan', admiMonth)], 0, y1 = 900)
rect(lockdown, 0, lockdown + 4, 900, col = 'grey87', border = NA)
text(lockdown, 940, 'First\nLockdown', adj = 0)
axis(1, xs, labels = F, pos = 0, tck = -0.01)
axis(1, xs[grepl('Jan', irid_monthly$admiMonth)], pos = 0, labels = F, tck = -0.02)
text(xs[grepl('Jan', irid_monthly$admiMonth)], -30, irid_monthly$admiMonth[grepl('Jan', irid_monthly$admiMonth)], srt = 60, adj = 1)
axis(2, 0:9 * 100, pos = 1, las = 2)
axis(4, yx2 * irid_monthly$N[1], yx2, pos = 48, las = 2)
segments(1, 900, x1 = 48)
segments(48, 0, y1 = 900)
mapply(linesb, 
       x = list(xs), 
       y = index3[, comps, with = F], 
       col = cols, 
       lwd = c(2, rep(1, length(comps)-1)),
       cex = c(1, rep(0.6, length(comps)-1)),
       lty = c(1, rep(3, length(comps)-1)))
mtext('Montly emergency admissions among patients aged 15-64', side = 2, line = 2.5)
mtext('Ratio of admissions\nvs. Jan 2018', side = 4, line = 3)
title(xlab = 'Date of admission', line = 4)
ys <- seq(600, 900, length.out = length(comps))
segments(54, ys, x1 = 57, col = cols)
points(rep(55.5, length(comps)), ys, col = cols, pch = 19)
text(57.5, ys, label, adj = 0)

dev.off()

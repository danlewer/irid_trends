# version of code generating black and white figure for print

library(data.table)
library(RColorBrewer)
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

# vectorized confidence intervals

vpt <- function(x, t, cn = c('rate', 'lower', 'upper'), FUN = poisson.test) {
  f <- function(x, t) if (is.na(x) | is.na(t)) c(NA, NA, NA) else c(x/t, FUN(x, t)$conf.int[1:2])
  a <- mapply(f, x, t, SIMPLIFY = T)
  `colnames<-`(t(a), cn)
}

#  ::::::::::::::::
#  Histogram of age
#  ................

age_hist <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_histogram_22feb2023.csv')
age_qs <- quantile(rep(age_hist$STARTAGE, age_hist$n), probs = c(0.25, 0.5, 0.75))

emf('age_histogram_22feb2023.emf', height = 5, width = 5, family = 'Tahoma')

par(mar = c(5, 5, 6, 1), xpd = NA)
plot(1, type = 'n', xlim = c(15, 65), ylim = c(0, 5500), axes = F, xlab = NA, ylab = NA)
axis(1, 15:65, labels = F, pos = 0, tck = -0.015)
axis(1, seq(15, 65, 5), labels = T, pos = 0, tck = -0.04)
axis(2, seq(0, 5500, 500), pos = 15, las = 2)
segments(15, 5500, x1 = 65)
segments(65, 0, y1 = 5500)
segments(age_qs, 0, y1 = 5500, lty = 2)
text(age_qs, 5700, paste0(c('Lower Q', 'Median', 'Upper Q'), ': ', age_qs), srt = 60, adj = 0)
rect(15:64, 0, 16:65, age_hist$n, col = brewer.pal(3, 'Set3')[1])
title(xlab = 'Age at admission', line = 2)
title(ylab = 'Number of admissions, 2002-2021')

dev.off()

#  ===============
#  Annual analysis
#  ---------------

# comparison between drds and admissions

drds <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/2021registrations.csv')
drds <- drds[, .(year = year, drds = drug_poisoning)]
ads <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_22feb2023.csv')
ads <- ads[, .(ad = sum(admissions)), year]
avd <- drds[ads, on = 'year'][order(year)]
days <- data.table(day = seq(from = as.Date('2002-01-01', origin = '1970-01-01'), to = as.Date('2022-12-31', origin = '1970-01-01'), by = 'day'))
days[, year := year(day)]
days <- days[, .(nd = .N), year]
avd <- days[avd, on = 'year']
avd <- cbind(avd, 
             vpt(x = avd$ad, t = avd$nd, cn = c('ad_rt', 'ad_upper', 'ad_lower')),
             vpt(x = avd$drds, t = avd$nd, cn = c('d_rt', 'd_upper', 'd_lower')))

round(avd[year %in% c(2003, 2011, 2019, 2020), c('year', 'ad_rt', 'ad_upper', 'ad_lower')], 2)

#  age-stratified counts by year (plus percentages)

yrAge <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_22feb2023.csv')
yrAge <- yrAge[, .(yearTotal = sum(admissions)), year][yrAge, on = 'year']
yrAge[, pc := admissions / yearTotal]
yrAge <- yrAge[order(year, age_group)]
yrAge <- days[yrAge, on = 'year']
yrAge[, rt := admissions / nd]
yrAge[, y2a := cumsum(rt), year]
yrAge[, y1a := shift(y2a, fill = 0L), year]
yrAge[, y2b := cumsum(pc), year]
yrAge[, y1b := shift(y2b, fill = 0L), year]
cols2 <- brewer.pal(5, 'Spectral')
cols2 <- brewer.pal(5, 'Greys')
yrAge[, cl := as.character(factor(age_group, c('15', '25', '35', '45', '55'), cols2))]

# age quantiles

age_quantiles <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_quantiles_22feb2023.csv')
age_quantiles <- age_quantiles[age_quantiles$region == 'England']
age_quantiles <- age_quantiles[, c(3:7)]

# homelessness

hln <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/homeless_22feb2023.csv')
hln[, total := yes + no]
hln <- cbind(hln, vpt(x = hln$yes, t = hln$total, FUN = prop.test, cn = c('prop', 'lower', 'upper')))

# Figure 1
# --------

tiff('Fig1_bw.tiff', height = 8, width = 5, units = 'in', res = 600, family = 'Tahoma')

par(mfrow = c(3, 1), mar = c(3, 5, 3, 12), xpd = NA)

# proportion by age

plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 1), axes = F, ylab = NA, xlab = NA)
rect(2002, 0, 2022, 1)
with(yrAge, rect(xleft = year, ybottom = y1b, xright = year + 1, ytop = y2b, col = cl))
axis(2, 0:5/5, paste0(0:5 * 20, '%'), pos = 2002, las = 2)
axis(1, 2002:2022, labels = F, pos = 0)
text(2002:2021 + 0.5, -1/20, labels = 2002:2021, srt = 60, adj = 1)
ys <- seq(0.2, 0.8, length.out = 6)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols2)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + max(ys)/6.5, 'Age at\nadmission', adj = 0)
text(2002, 1.1, 'A: Proportion of admissions by age group', adj = 0, cex = 1.05)
title(ylab = 'Percent of patients', line = 2.5)

# Compared to DRDs

cols <- brewer.pal(4, 'Paired')
cols <- brewer.pal(5, 'Greys')[c(2, 5)]

plot(1, type = 'n', ylim = c(0, 20), xlim = c(2002, 2021), axes = F, xlab = NA, ylab = NA)
rect(2002, 0, 2021, 20, col = 'white')
segments(2002, 0:4 * 5, 2021, col = 'grey80', lwd = 0.6)
rect(2002, 0, 2021, 20)
axis(1, 2002:2021, labels = F, pos = 0)
text(2002:2021, -1.5, labels = 2002:2021, srt = 60, adj = 1)
axis(2, 0:4 * 5, las = 2, pos = 2002)

with(avd[!is.na(drds)], {
  polygon(x = c(year, rev(year)), y = c(d_lower, rev(d_upper)), col = cols[1], border = NA)
  lines(year, d_rt, col = cols[2])
  points(year, d_rt, col = cols[2], pch = 0)
})
with(avd, {
  polygon(x = c(year, rev(year)), y = c(ad_lower, rev(ad_upper)), col = cols[1], border = NA)
  lines(year, ad_rt, col = cols[2])
  points(year, ad_rt, col = cols[2], pch = 4)
})

title(ylab = 'Number per day', line = 2.5)
ys <- c(7, 13)
segments(x0 = 2022, y0 = ys, x1 = 2023, col = cols[2])
points(c(2022.5, 2022.5), ys, pch = c(0, 4))
text(2023.5, ys, c('Deaths due to\ndrug poisoning', 'Admissions due to\ninjection associated\ninfections'), adj = 0)

text(2002, 1.1 * 20, 'B: Compared to deaths due to drug poisoning in England', adj = 0, cex = 1.05)

# Homelessness

#cols <- brewer.pal(4, 'Paired')
plot(1, type = 'n', xlim = c(2002, 2021), ylim = c(0, 0.2), axes = F, xlab = NA, ylab = NA)
rect(2002, 0, 2021, 0.2, col = 'white')
segments(2002, 0:4 * 5 / 100, x1 = 2021, col = 'grey80', lwd = 0.6)
rect(2002, 0, 2021, 0.2)
axis(1, 2002:2021, labels = F, pos = 0)
text(2002:2021, -0.02, labels = 2002:2021, srt = 60, adj = 1)
axis(2, 0:4 * 5 / 100, paste0(0:4 * 5, '%'), pos = 2002, las = 2)
with(hln, {
  polygon(x = c(year, rev(year)), y = c(lower, rev(upper)), col = cols[1], border = NA)
  lines(year, prop, col = cols[2])
  points(year, prop, col = cols[2], pch = 19)
})
title(ylab = 'Proportion of admissions', line = 2.5)
text(2002, 1.1 * 0.2, 'C: Proportion of admissions where homelessness is coded', adj = 0, cex = 1.05)

dev.off()

# additional age plots for supplementary
# --------------------------------------

# numbers by age

emf('FigSX_age1.emf', height = 4, width = 7, family = 'Tahoma')

par(mar = c(3, 4, 1, 8), xpd = NA)
plot(1, type = 'n', xlim = c(2002, 2021), ylim = c(0, 20), axes = F, ylab = NA, xlab = NA)
segments(2002, 0:4 * 5, 2022, col = 'grey80', lwd = 0.6)
rect(2002, 0, 2022, 20)
with(yrAge, rect(xleft = year, ybottom = y1a, xright = year + 1, ytop = y2a, col = cl))
axis(2, 0:4 * 5, pos = 2002, las = 2)
axis(1, 2002:2022, labels = F, pos = 0)
text(2002:2021 + 0.5, -1, labels = 2002:2021, srt = 60, adj = 1)
ys <- seq(5, 15, length.out = 6)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols2)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + max(ys)/7.5, 'Age at\nadmission', adj = 0)
title(ylab = 'Number per day')

dev.off()

# distribution of age

emf('FigSX_age2.emf', height = 4, width = 7, family = 'Tahoma')

par(mar = c(3, 4, 1, 8), xpd = NA)
col <- brewer.pal(5, 'Spectral')[4]
gap <- 0.3
ys <- seq(30, 50, length.out = 5)
plot(1, type = 'n', xlim = c(2002, 2021), ylim = c(20, 60), axes = F, xlab = NA, ylab = NA)
segments(2002, 2:6 * 10, x1 = 2022, col = 'grey80', lwd = 0.6)
rect(2002, 20, 2022, 60)
arrows(x0 = 2002:2021 + 0.5, y0 = age_quantiles$q0.05, y1 = age_quantiles$q0.95, length = 0.03, code = 3, angle = 90)
rect( 
  xleft = 2002:2021 + 0.5 - gap, 
  ybottom = age_quantiles$q0.25, 
  xright = 2002:2021 + 0.5 + gap,
  ytop = age_quantiles$q0.75,
  col = col)
segments(x0 = 2002:2021 + 0.5 - gap, y0 = age_quantiles$q0.5, x1 = 2002:2021 + 0.5 + gap)
axis(1, 2002:2022, labels = F, pos = 20)
text(2002:2021 + 0.5, 18, labels = 2002:2021, srt = 60, adj = 1)
axis(2, 2:6 * 10, pos = 2002, las = 2)
arrows(2023.5, y0 = ys[1], y1 = ys[5], length = 0.05, code = 3, angle = 90)
rect(2023.5 - gap, ys[2], 2023.5 + gap, ys[4], col = col)
segments(x0 = 2023.5 - gap, y0 = ys[3], x1 = 2023.5 + gap)
text(2023.5 + gap + 0.5, ys, c(0.05, 0.25, 0.5, 0.75, 0.95), adj = 0)
text(2023.5, max(ys) + max(ys)/6.5, 'Quantile\nof age', adj = 0)
title(ylab = 'Age at admission')

dev.off()

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

# vectorized confidence intervals

vpt <- function(x, t, cn = c('rate', 'lower', 'upper'), FUN = poisson.test) {
  a <- mapply(FUN, x, t, SIMPLIFY = F)
  a <- sapply(a, function (x) c(x$estimate, x$conf.int[1:2]))
  `colnames<-`(t(a), cn)
}

#  ::::::::::::::::
#  Histogram of age
#  ................

age_hist <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_histogram_17oct2022.csv')
age_qs <- quantile(rep(age_hist$STARTAGE, age_hist$n), probs = c(0.25, 0.5, 0.75))

emf('age_histogram_10nov2022.emf', height = 5, width = 5, family = 'Tahoma')

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
ads <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_17oct2022.csv')
ads <- ads[, .(ad = sum(admissions)), year]
avd <- drds[ads, on = 'year'][order(year)]
days <- data.table(day = seq(from = as.Date('2002-01-01', origin = '1970-01-01'), to = as.Date('2021-12-31', origin = '1970-01-01'), by = 'day'))
days[, year := year(day)]
days <- days[, .(nd = .N), year]
avd <- days[avd, on = 'year']
avd <- cbind(avd, 
             vpt(x = avd$ad, t = avd$nd, cn = c('ad_rt', 'ad_upper', 'ad_lower')),
             vpt(x = avd$drds, t = avd$nd, cn = c('d_rt', 'd_upper', 'd_lower')))

round(avd[year %in% c(2003, 2011, 2019, 2020), c('year', 'ad_rt', 'ad_upper', 'ad_lower')], 2)


#  age-stratified counts by year (plus percentages)

yrAge <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_17oct2022.csv')
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
yrAge[, cl := as.character(factor(age_group, c('15', '25', '35', '45', '54'), cols2))]

# age quantiles

age_quantiles <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_quantiles_17oct2022.csv')
age_quantiles <- age_quantiles[age_quantiles$region == 'England']
age_quantiles <- age_quantiles[, c(3:7)]

# homelessness

hln <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/homeless_14nov2022.csv')
hln[, total := yes + no]
hln <- cbind(hln, vpt(x = hln$yes, t = hln$total, cn = c('prop', 'lower', 'upper')))

#emf('Figure1.emf', height = 9, width = 4, family = 'Franklin Gothic Book')
png('Figure1_14nov2022.png', height = 9, width = 9, family = 'Tahoma', res = 300, units = 'in')

par(mar = c(3, 5, 4, 5), xpd = NA, mfrow = c(2, 2))
xl <- seq(2005, 2020, 5)

# compared to DRDs

cols <- brewer.pal(4, 'Paired')

plot(1, type = 'n', ylim = c(0, 20), xlim = c(2002, 2021), axes = F, xlab = NA, ylab = NA)
axis(1, 2002:2021, labels = F, pos = 0)
text(xl, -1, labels = xl, srt = 60, adj = 1)
axis(2, 0:4 * 5, las = 2, pos = 2002)
segments(2002, 0:4 * 5, 2021, col = 'grey80', lwd = 0.6)

with(avd, {
  polygon(x = c(year, rev(year)), y = c(d_lower, rev(d_upper)), col = cols[1], border = NA)
  lines(year, d_rt, col = cols[2])
  points(year, d_rt, col = cols[2], pch = 19)
  polygon(x = c(year, rev(year)), y = c(ad_lower, rev(ad_upper)), col = cols[3], border = NA)
  lines(year, ad_rt, col = cols[4])
  points(year, ad_rt, col = cols[4], pch = 19)
})

text(2002, 20 * 1.1, 'A: Hospital admissions due to injection-associated\ninfections, compared to deaths due to drug poisoning', adj = 0)
title(ylab = 'Rate per day')

ys <- c(8.5, 11.5)
segments(x0 = 2021.5, y0 = ys, x1 = 2022.5, col = cols[c(2, 4)])
text(2023, ys, c('Deaths', 'Infections'), adj = 0)

# numbers by age

plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 20), axes = F, ylab = NA, xlab = NA)
segments(2002, 0:4 * 5, 2022, col = 'grey80', lwd = 0.6)
with(yrAge, rect(xleft = year, ybottom = y1a, xright = year + 1, ytop = y2a, col = cl))
axis(2, 0:4 * 5, pos = 2002, las = 2)
axis(1, 2002:2022, labels = F, pos = 0)
text(xl + 0.5, -1, labels = xl, srt = 60, adj = 1)
ys <- seq(5, 15, length.out = 6)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols2)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + max(ys)/10, 'Age at\nadmission', adj = 0)
text(2002, 20 * 1.1, 'B: Hospital admissions due to injection-associated\ninfections, by age at admission', adj = 0)
title(ylab = 'Rate per day')

# distribution of age

col <- brewer.pal(5, 'Spectral')[4]
gap <- 0.3
ys <- seq(30, 50, length.out = 5)
plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(20, 60), axes = F, xlab = NA, ylab = NA)
segments(2002, 2:6 * 10, x1 = 2022, col = 'grey80', lwd = 0.6)
arrows(x0 = 2002:2021 + 0.5, y0 = age_quantiles$q0.05, y1 = age_quantiles$q0.95, length = 0.03, code = 3, angle = 90)
rect( 
  xleft = 2002:2021 + 0.5 - gap, 
  ybottom = age_quantiles$q0.25, 
  xright = 2002:2021 + 0.5 + gap,
  ytop = age_quantiles$q0.75,
  col = col)
segments(x0 = 2002:2021 + 0.5 - gap, y0 = age_quantiles$q0.5, x1 = 2002:2021 + 0.5 + gap)
axis(1, 2002:2022, labels = F, pos = 20)
text(xl + 0.5, 18, labels = xl, srt = 60, adj = 1)
axis(2, 2:6 * 10, pos = 2002, las = 2)
arrows(2023.5, y0 = ys[1], y1 = ys[5], length = 0.05, code = 3, angle = 90)
rect(2023.5 - gap, ys[2], 2023.5 + gap, ys[4], col = col)
segments(x0 = 2023.5 - gap, y0 = ys[3], x1 = 2023.5 + gap)
text(2023.5 + gap + 0.5, ys, c(0.05, 0.25, 0.5, 0.75, 0.95), adj = 0)
text(2023.5, max(ys) + max(ys)/10, 'Quantile\nof age', adj = 0)
text(2002, 60 + 40 * 0.1, 'C: Distribution of age at hospital admission\ndue to injection-associated infections', adj = 0)
title(ylab = 'Age at admission')

# homelessness

cols <- brewer.pal(4, 'Paired')

plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 0.2), axes = F, xlab = NA, ylab = NA)
axis(1, 2002:2022, labels = F, pos = 0)
text(xl, -0.01, labels = xl, srt = 60, adj = 1)
axis(2, 0:4 * 5 / 100, paste0(0:4 * 5, '%'), pos = 2002, las = 2)
segments(2002, 0:4 * 5 / 100, x1 = 2022, col = 'grey80', lwd = 0.6)
with(hln, {
  polygon(x = c(year, rev(year)), y = c(lower, rev(upper)), col = cols[1], border = NA)
  lines(year, prop, col = cols[2])
  points(year, prop, col = cols[2], pch = 19)
})
text(2002, 0.2 * 1.1, 'D: Admissions due to injection-associated\ninfections where homelessness is coded', adj = 0)
title(ylab = 'Proportion of admissions')

dev.off()

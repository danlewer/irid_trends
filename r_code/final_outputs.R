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

age_hist <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_histogram_17oct2022.csv')
age_qs <- quantile(rep(age_hist$STARTAGE, age_hist$n), probs = c(0.25, 0.5, 0.75))

emf('age_histogram_17oct2022.emf', height = 5, width = 5, family = 'Franklin Gothic Book')

par(mar = c(5, 5, 6, 1), xpd = NA)
plot(1, type = 'n', xlim = c(15, 65), ylim = c(0, 5500), axes = F, xlab = NA, ylab = NA)
axis(1, 15:65, labels = F, pos = 0, tck = -0.015)
axis(1, seq(15, 65, 5), labels = T, pos = 0, tck = -0.04)
axis(2, seq(0, 5500, 500), pos = 15, las = 2)
segments(15, 5500, x1 = 65)
segments(65, 0, y1 = 5500)
segments(age_qs, 0, y1 = 5500, lty = 2)
text(age_qs, 5700, paste0(c('Lower Q', 'Median', 'Upper Q'), ': ', age_qs), srt = 60, adj = 0)
rect(15:64, 0, 16:65, age_hist$n, col = viridis(6)[5])
title(xlab = 'Age at admission', line = 2)
title(ylab = 'Number of admissions, 2002-2021')

dev.off()

#  ======================================================
#  Compare DRDs with injecting-related infections by year
#  ------------------------------------------------------

drds <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/drug_poisoning_registrations.csv')
admissions <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_17oct2022.csv')

setnames(drds, 'Year of death registration', 'year')
drd_summary <- drds[Substance == '01 All drug poisonings' & `Usual residence name` == 'England' & year >= 1998, .(drds = sum(Deaths)), year]
drd_summary <- rbind(data.table(year = 2021, drds = 4859), drd_summary)
#drd_summary <- drds[Substance == '02 Any opiate (includes unspecified opiates, excludes paracetamol compounds)' & Usual.residence.name == 'England' & Year.of.death.registration >= 1998, .(drds = sum(Deaths)), Year.of.death.registration]
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

#  =============================
#  Age-stratified counts by year
#  -----------------------------

yrAge <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/admissions_by_year_and_age_group_17oct2022.csv')
# % change by year
yrAge[, sum(admissions), year][, cbind(year, V1, c(0, diff(V1)) / V1 * 100)]
yrAge <- yrAge[, .(yearTotal = sum(admissions)), year][yrAge, on = 'year']
yrAge[, pc := admissions / yearTotal]
yrAge <- yrAge[order(year, age_group)]

yrAge <- yrAge[yrAge$year >= 2002 & yrAge$year <= 2021,]

yrAgeCum <- with(yrAge, xtabs(admissions ~ age_group + year))
yrAgeCum <- rbind(0, yrAgeCum)
yrAgeCum <- apply(yrAgeCum, 2, cumsum)
xleft <- matrix(rep(2002:2021, 5), ncol = length(2002:2021), byrow = T)

age_quantiles <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/age_quantiles_17oct2022.csv')
age_quantiles <- age_quantiles[age_quantiles$region == 'England']
age_quantiles <- age_quantiles[, c(3:7)]

cols <- viridis(5)

# percent by age group

yrAge[, bct := cumsum(pc), year]
yrAge[, bcb := shift(bct, fill = 0L), year]
yrAge[, cl := as.character(factor(age_group, c('15', '25', '35', '45', '54'), cols))]

#emf('Figure1.emf', height = 9, width = 4, family = 'Franklin Gothic Book')
png('Figure1.png', height = 9, width = 5, family = 'Franklin Gothic Book', res = 300, units = 'in')

par(mar = c(3, 5, 1, 7), xpd = NA, mfrow = c(3, 1))

# compared to DRDs

plot(1, type = 'n', ylim = c(0, ymax), xlim = c(2001, 2022), axes = F, xlab = NA, ylab = NA)

with(admissions_vs_drds[year >= 1998], {
  lines(year, admissions_index, col = cols[1])
  points(year, admissions_index, pch = 19, col = cols[1])
  lines(year, drds_index, pch = 19, col = cols[4])
  points(year, drds_index, pch = 19, col = cols[4])
})

axis(1, 2002:2021, labels = F, pos = 0)
segments(2001, 0, x1 = 2022)
text(2002:2021, -0.1, labels = 2002:2021, srt = 60, adj = 1)
axis(2, yx1/ref_year[, admissions], yx1, las = 2, pos = 2001, col = cols[1], col.axis = cols[1])
axis(4, yx2/ref_year[, drds], yx2, las = 2, pos = 2022, col = cols[4], col.axis = cols[4])

# segments(2001, 0, y1 = ymax, col = cols[1])
# segments(2001, ymax, x1 = 2022)
# segments(2022, 0, y1 = ymax, col = cols[4])

mtext('Hospital admissions\ndue to IRID', side = 2, line = 3, col = cols[1], cex = 0.7)
mtext('Deaths due to drug poisoning', side = 4, line = 3, col = cols[4], cex = 0.7)
text(2011.5, ymax, 'A: Count of IRID compared to\ncount of deaths due to drug poisoning', font = 2)

# numbers by age

ys <- seq(1875, 5625, length.out = 6)
plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 7000), axes = F, ylab = 'Hospital admissions\ndue to IRID', xlab = NA)
rect(xleft = xleft, ybottom = yrAgeCum[-6,], xright = xleft + 1, ytop = yrAgeCum[-1,], col = cols)
axis(2, 0:7 * 1000, pos = 2002, las = 2)
axis(1, 2002:2022, labels = F, pos = 0)
# segments(2022, 0, y1 = 7000)
# segments(2002, 7000, x1 = 2022)
text(2002:2021 + 0.5, -250, labels = 2002:2021, srt = 60, adj = 1)
rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols)
text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
text(2023, max(ys) + max(ys)/10, 'Age at\nadmission', adj = 0)
text(2012, 7000, 'B: Count of IRID by age', font = 2)

# # percent by age
# 
# ys <- seq(0.25, 0.75, length.out = 6)
# plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(0, 1), axes = F, xlab = NA, ylab = NA)
# with(yrAge, rect(year, bcb, year + 1, bct, col = cl))
# axis(1, 2002:2022, labels = F, pos = 0)
# text(2002:2021 + 0.5, -0.05, labels = 2002:2021, srt = 60, adj = 1)
# axis(2, 1:5/5, paste0(1:5 * 20, '%'), pos = 2002, las = 2)
# rect(2023, ys[-1], 2024, ys[-length(ys)], col = cols)
# text(2024.5, ys[-length(ys)] + diff(ys)/2, c('15-24', '25-34', '35-44', '45-54', '55-64'), adj = 0)
# text(2023, max(ys) + 300, 'Age at\nadmission', adj = 0)
# title(ylab = 'Percent of patients')
# text(2023, max(ys) + 0.1, 'Age at\nadmission', adj = 0)

# distribution of age

gap <- 0.3
ys <- seq(30, 50, length.out = 5)
plot(1, type = 'n', xlim = c(2002, 2022), ylim = c(20, 60), axes = F, xlab = 'Year', ylab = 'Age at admission')
arrows(x0 = 2002:2021 + 0.5, y0 = age_quantiles$q0.05, y1 = age_quantiles$q0.95, length = 0.03, code = 3, angle = 90)
rect( 
  xleft = 2002:2021 + 0.5 - gap, 
  ybottom = age_quantiles$q0.25, 
  xright = 2002:2021 + 0.5 + gap,
  ytop = age_quantiles$q0.75,
  col = cols[4])
segments(x0 = 2002:2021 + 0.5 - gap, y0 = age_quantiles$q0.5, x1 = 2002:2021 + 0.5 + gap)
axis(1, 2002:2022, labels = F, pos = 20)
text(2002:2021 + 0.5, 18, labels = 2002:2021, srt = 60, adj = 1)
axis(2, seq(20, 60, 10), pos = 2002, las = 2)
# segments(2022, 20, y1 = 60)
# segments(2002, 60, x1 = 2022)
arrows(2023.5, y0 = ys[1], y1 = ys[5], length = 0.05, code = 3, angle = 90)
rect(2023.5 - gap, ys[2], 2023.5 + gap, ys[4], col = cols[4])
segments(x0 = 2023.5 - gap, y0 = ys[3], x1 = 2023.5 + gap)
text(2023.5 + gap + 0.5, ys, c(0.05, 0.25, 0.5, 0.75, 0.95), adj = 0)
text(2023.5, max(ys) + max(ys)/10, 'Quantile\nof age', adj = 0)
text(2012, 60, 'C: Distribution of age at\nhospital admission due to IRID', font = 2)

dev.off()

#  ::::::::::::::::::::
#  time series analysis
#  ....................

ss <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/summary_tables/counts_per_month_27oct2022.csv')

# dx <- c('irid', 'sstvi', 'invasive', 'skin_staph', 'other_staph', 'skin_strep', 'other_strep', 'f11', 'tcodes')
# titles <- c('All IRID', 'SSTVI', 'Invasive infections', 'Staphylocccal (skin)', 'Staphylococcal (other)', 'Streptococcal (skin)', 'Streptococcal (other)', 'Opioid-related (F11)', 'Opioid poisoning (T codes)')

dx <- c('irid', 'cocci', 'tcodes')
titles <- c('A: Opioid-injection associated infections', 'B: Other skin infections', 'C: Opioid poisoning')

sf <- function (var = 'abscess', data = ss, pol = 3) {
  f1 <- as.formula(paste0(var, '~poly(time, ', pol, ')+mth+covid'))
  f2 <- as.formula(paste0(var, '~poly(time, ', pol, ')+covid'))
  m1 <- glm(f1, data = data, family = 'poisson')
  m2 <- glm(f2, data = data, family = 'poisson')
  nd <- data.frame(time = ss[!is.na(get(var)), max(time)], mth = month.abb, covid = T)
  p1 <- predict(m1, newdata = nd, type = 'link', se.fit = T)
  p1_point <- m1$family$linkinv(p1$fit)
  p1_lower <- m1$family$linkinv(p1$fit - qnorm(0.975) * p1$se.fit)
  p1_upper <- m1$family$linkinv(p1$fit + qnorm(0.975) * p1$se.fit)
  p2 <- predict(m1, newdata = data[!is.na(get(var))], type = 'response')
  min_month <- month.abb[which.min(p1_point)]
  max_month <- month.abb[which.max(p1_point)]
  p2lm <- factor(data$mth, c(min_month, setdiff(month.abb, min_month)))
  f3 <- as.formula(paste0('data$', var, '~poly(data$time, ', pol, ')+p2lm+data$covid'))
  p2l <- glm(f3, family = 'poisson')
  p2l <- exp(cbind(p2l$coef, confint(p2l)))
  p2l <- p2l[grepl('p2l', row.names(p2l)),]
  p2l <- p2l[which.max(p2l[,1]),]
  p2l <- format(round(p2l, 2), digits = 2, nsmall = 2)
  p2l <- paste0(p2l[1], ' (', p2l[2], '-', p2l[3], ')')
  evs <- anova(m1, m2, test = 'LRT')$`Pr(>Chi)`[2]
  evs <- round(evs, 4)
  evs <- if(evs == 0) '<0.0001' else evs
  covid <- tail(exp(cbind(coef(m1), confint(m1))), 1)
  covid <- format(round(covid, 2), digits = 2, nsmall = 2)
  covid <- paste0(covid[1], ' (', covid[2], '-', covid[3], ')')
  list(summary = c(max_month = max_month, min_month = min_month, p2l = p2l, evidence = evs, covid = covid),
       predicated_monthly = rbind(point = p1_point, lower = p1_lower, upper = p1_upper),
       predicted_daily = p2)
}

irid_season <- lapply(dx, sf, pol = 2)
irid_season_table <- cbind(diagnosis = dx, t(sapply(irid_season, function (x) x[[1]])))
fwrite(irid_season_table, 'irid_season_27oct2022.csv')

#  ===========
#  month plots
#  -----------

mth_vals <- lapply(irid_season, function (x) x[[2]])
names(mth_vals) <- dx
cols <- brewer.pal(4, 'Set2')
cols <- c(rep(cols[1], 3), rep(cols[2:4], each = 2))

ymax <- sapply(mth_vals, max) * 1.3

emf('month_patterns_27oct2022.emf', height = 8, width = 7, family = 'Franklin Gothic Book')

layout(mat = matrix(c(1:4, 6, 8, 5, 7, 9), ncol = 3, byrow = T))

par(mar = c(2, 3, 0, 0))
for(i in seq_along(dx)) {
  if (is.na(dx[i])) {
    plot(1, type = 'n', axes = F, xlab = NA, ylab = NA)
  } else {
    plot(1, type = 'n', xlim = c(0, 12), ylim = c(0, ymax[i]), axes = F, xlab = NA, ylab = NA)
    rect(0:11, 0, 1:12, mth_vals[[i]][1,], col = cols[i])
    arrows(0:11 + 0.5, mth_vals[[i]][2,], y1 = mth_vals[[i]][3,], code = 3, angle = 90, length = 0.05)
    axis(1, pos = 0, 0:12, labels = F)
    axis(1, pos = 0, 0:11 + 0.5, substr(month.abb, 0, 1), tick = F)
    axis(2, pos = 0, las = 2)
    rect(0, 0, 12, ymax[i])
    text(0.5, ymax[i] * 0.95, titles[i], adj = c(0, 1))
  }
}

dev.off()


#  =================
#  time-series plots
#  ----------------

ts_vals <- lapply(irid_season, function (x) x[[3]])
names(ts_vals) <- dx
ymax <- sapply(ts_vals, max, na.rm = T) * 1.1

jan_lines <- ss[mth == 'Jan', time]
cov_line <- ss[mth == 'Mar' & year == 2020, time]
red2 <- brewer.pal(3, 'Set1')[1]


#emf('time_series_plot_17oct2022.emf', height = 3, width = 7, family = 'Franklin Gothic Book')
png('Figure2.png', height = 3, width = 7, res = 600, units = 'in', family = 'Franklin Gothic Book')

par(mar = c(0, 3, 0, 0), oma = c(5, 3, 2, 0), mfrow = c(1, 3), xpd = NA)

for (i in seq_along(dx)) {
  plot(1, type = 'n', ylim = c(0, ymax[i]), xlim = c(1, 120), xlab = NA, ylab = NA, axes = F)
  rect(1, 0, 120, ymax[i])
  segments(cov_line, 0, y1 = ymax[i], col = red2, lwd = 0.8)
  segments(jan_lines, 0, y1 = ymax[i], col = 'grey50', lwd = 0.5)
  #segments(0, ymax[i] * 0.85, x1 = 120)
  points(ss[, time], ss[, get(dx[i])], cex = 0.6, lwd = 0.5)
  lines(seq_along(ts_vals[[i]]), ts_vals[[i]], lwd = 0.7)
  axis(2, pos = 1, yax(ymax[i], ntick = 6), las = 2)
  text(1, ymax[i] * 1.05, titles[i], adj = c(0, 0))
  axis(1, c(jan_lines, 120), labels = F, pos = 0)
  text(jan_lines + 6, ymax[i] * -0.19, 2012:2021, adj = 0, srt = 90)
}

mtext('Number of emergency hospital\nadmissions in England', side = 2, outer = T, cex = 0.7, line = 1)
mtext('Date of admission (tick mark = January)', side = 1, outer = T, cex = 0.7, line = 3.5)

dev.off()

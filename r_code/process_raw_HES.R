# secure folder location
setwd("url")

# =======================
# libraries and functions
# -----------------------

# load libraries
library(data.table)
library(lubridate) # for date formatting
library(RColorBrewer) # more color palettes
library(extrafont) # for fonts in charts
library(tsModel) # for 'harmonic'
library(devEMF) # for enhanced metafile graphic device (vector graphics)

# function for setting y-axis tick marks
yax <- function(x, tickabove = F, ntick = 5) {
  l <- c(c(1, 2, 4, 5, 25) %o% 10^(0:8))
  d <- l[which.min(abs(x/ntick - l))]
  d <- 0:(ntick+1) * d
  i <- findInterval(x, d)
  if (tickabove) {i <- i + 1}
  d[seq_len(i)]
}

# format HES dates
format_date <- function (x) {
  a <- suppressWarnings(cbind(dmy(x), ymd(x)))
  a <- a[cbind(seq_len(nrow(a)), max.col(!is.na(a)))]
  as.Date(a, origin = '1970-01-01')
}

# vectorised poisson confidence intervals
vpt <- function(x, t, form = F, digs = 2, ...) {
  f <- function(xl, tl) {
    if (is.na(xl) | is.na(tl)) return(c(0, 0, 0))
    y <- poisson.test(xl, tl, ...)
    c(xl/tl, y$conf.int[1:2])
  }
  res <- mapply(f, xl = x, tl = t)
  return(if (form) {
    res <- format(round(res, digs), digits = digs, nsmall = digs)
    res <- apply(res, 2, function(x) paste0(x[1], '(', x[2], '-', x[3], ')'))
    res <- gsub(' ', '', res)
    gsub('\\(', ' (', res)
  } else {
    res
  })
}

# ICD10 codes
icd10 <- list(
  abscess = 'L02',
  cellulitis = 'L03',
  other_ssti = c('A480', 'L08', 'L97', 'L984', 'R02'),
  phlebitis = 'I80',
  endocarditis = c('B376', 'I33', 'I38', 'I39'),
  septicaemia = c('A40', 'A41', 'R572', 'B377'),
  osteo = c('M86', 'M00', 'M01', 'M02', 'M03', 'M465'),
  nf = 'M726'
)

# ===========================
# read patient-level HES data
# ---------------------------

d_raw <- fread("irid_hes_extract_18feb2023.csv")
g <- fread('irid_hes_extract_diagnoses_18feb2023.csv')
r <- fread('irid_hes_extract_all_admissions_18feb2023.csv')
d <- copy(d_raw)

# ================
# format variables
# ----------------

# age
d[, STARTAGE := as.integer(STARTAGE)]

# sex
d[, SEX := factor(SEX, c(1, 2, 9, 0), c('male', 'female or not known', 'female or not known', 'female or not known'))]

# region
d[, region1 := factor(RESGOR, c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'U', 'Y', 'NULL', 'S', 'W', 'X', 'Z'), c('North East', 'North West', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East', 'South West', 'No fixed abode', 'Unknown', 'Unknown', 'Not England', 'Not England', 'Not England', 'Not England'))]
d[, region2 := factor(RESGOR_ONS, c('E12000001', 'E12000002', 'E12000003', 'E12000004', 'E12000005', 'E12000006', 'E12000007', 'E12000008', 'E12000009', 'M99999999', 'NULL', 'U', 'W99999999', 'X', 'Y'), c('North East', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East', 'South West', 'Not England', 'Unknown', 'No fixed abode', 'Not England', 'Not England', 'Unknown'))]
d[, region := fifelse(region1 == 'Unknown', as.character(region2), as.character(region1))]

# dates
date_cols <- c('ADMIDATE', 'DISDATE')
d[, (date_cols) := lapply(.SD, format_date), .SDcols = date_cols]
d[, year := year(ADMIDATE)]
year_lims <- seq(2002, 2017, 5)
d[, period := findInterval(year, year_lims)]
d[, period := factor(period, seq_along(year_lims), year_lims)]

# admission and discharge types
d$epo <- 'other'
d$epo[d$ADMIMETH %in% c(21:25, '2A', '2B', '2C', '2D', 28)] <- 'emergency'
d$epo[d$ADMIMETH %in% 11:13] <- 'planned'  
d[, epo := factor(epo, c('emergency', 'planned', 'other'))]
d[, admiMonth := paste0(month(ADMIDATE, label = T, abbr = T), ' ', year(ADMIDATE))]
d[, admiMonth := factor(admiMonth, c(outer(month.abb, paste0(' ', 1997:2022), paste0)))]

# primary cause
diagpos <- sapply(sapply(icd10, paste0, collapse = '|'), grepl, d$DiagCode4)
d[, diagnosis := names(icd10)[max.col(diagpos)]]
d$diagnosis[rowSums(diagpos) == 0] <- 'missing'
d[, diagnosis := factor(diagnosis, c(names(icd10), 'missing'))]

# Admission ID & discharge method
d <- d[year >= 2002 & year <= 2022] # TOKEN_PERSON_ID missing before 2001
make_ad_ID <- unique(d[, c('TOKEN_PERSON_ID', 'ADMIDATE')])
make_ad_ID[, adID := .I]
d <- make_ad_ID[d, on = c('TOKEN_PERSON_ID', 'ADMIDATE')]
d <- d[, .(maxEPIORDER = max(EPIORDER)), adID][d, on = 'adID']
disMeth <- unique(d[EPIORDER == maxEPIORDER, .(adID = adID, finalDis = DISMETH)])
disMeth[, finalDis := factor(finalDis, c(4, 2, 1, 3, 8, 9), c('died', 'DAMA', 'normal', 'other', 'other', 'other'))]
disMeth <- disMeth[, .(finalDis = min(as.integer(finalDis))), adID]
disMeth[, finalDis := factor(finalDis, 1:4, c('died', 'DAMA', 'normal', 'other'))]
d <- disMeth[d, on = 'adID']

# dummy variable
d[, total := 1]
d[, total2 := 'total']

# ==================
# impute deprivation
# ------------------

imd <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/imd2015.csv')
r[, ADMIDATE := format_date(ADMIDATE)]
imdi <- r[, c('TOKEN_PERSON_ID', 'ADMIDATE', 'IMD04RK', 'LSOA11')]
imdi <- imd[, c('LSOA11', 'decile')][imdi, on = 'LSOA11']
imdi[, IMD04RK := as.numeric(IMD04RK)]
imdi[, imd5 := findInterval(IMD04RK, seq(0, 32482, length.out = 6))]
imdi[, imd5_2 := ceiling(decile/2)]
imdi[, imd5 := fifelse(is.na(imd5_2), imd5, imd5_2)]
imdi <- imdi[!is.na(imd5)]
imdi <- imdi[, .(max_admidate = max(ADMIDATE, na.rm = T)), TOKEN_PERSON_ID][imdi, on = 'TOKEN_PERSON_ID']
imdi <- imdi[ADMIDATE == max_admidate, .(imd5_impute = max(imd5)), TOKEN_PERSON_ID]

d[, IMD04RK := as.numeric(IMD04RK)]
d[, imd5 := findInterval(IMD04RK, seq(0, 32482, length.out = 6))]
d <- imdi[d, on = 'TOKEN_PERSON_ID']
d[, imd5 := fifelse(is.na(imd5), imd5_impute, imd5)]
d$imd5[is.na(d$imd5)] <- 'missing'
d[, imd5 := factor(imd5, c(1:5, 'missing'), c('1 - most deprived', 2:4, '5 - least deprived', 'missing'))]
rm(imdi, imd)
d[, imd5_impute := NULL]

# ================
# impute ethnicity
# ----------------

r[, ETHNOS := gsub(' ', '', ETHNOS)]
ethi <- r[!(ETHNOS %in% c('Z', '99', '9')), c('TOKEN_PERSON_ID', 'ADMIDATE', 'ETHNOS')]
ethi <- ethi[, .(max_admidate = max(ADMIDATE, na.rm = T)), TOKEN_PERSON_ID][ethi, on = 'TOKEN_PERSON_ID']
ethi <- ethi[ADMIDATE == max_admidate]
set.seed(4)
ethi[, rand := rnorm(.N)]
ethi <- ethi[order(TOKEN_PERSON_ID, rand)]
ethi[, id := rowid(TOKEN_PERSON_ID)]
ethi <- ethi[id == 1, .(TOKEN_PERSON_ID = TOKEN_PERSON_ID, eth_impute = ETHNOS)]

d[, ETHNOS := gsub(" ", "", ETHNOS)]
d$ETHNOS[d$ETHNOS %in% c('Z', '99', '9')] <- NA
d <- ethi[d, on = 'TOKEN_PERSON_ID']
d[, ETHNOS := fifelse(is.na(ETHNOS), eth_impute, ETHNOS)]

d$ETHNOS[d$ETHNOS %in% c('A', 'B', 'C', '0')] <- 'White'
d$ETHNOS[d$ETHNOS %in% c('D', 'E', 'F', 'G')] <- 'Mixed'
d$ETHNOS[d$ETHNOS %in% c('H', 'J', 'K', 'L', 4:6)] <- 'Asian'
d$ETHNOS[d$ETHNOS %in% c('M', 'N', 'P', 1:3)] <- 'Black'
d$ETHNOS[d$ETHNOS %in% c('R', 'S', 'Z', 'X', 7:9, 99) | is.na(d$ETHNOS)] <- 'Other or unknown'
d[, ETHNOS := factor(ETHNOS, c('White', 'Black', 'Asian', 'Mixed', 'Other or unknown'))]
rm(ethi)
d[, eth_impute := NULL]

# ===============
# impute STARTAGE
# ---------------

r[, STARTAGE := as.numeric(STARTAGE)]
agei <- r[!is.na(STARTAGE), c('TOKEN_PERSON_ID', 'ADMIDATE', 'STARTAGE')]
agei[, yearBirth := year(ADMIDATE) - STARTAGE]
agei <- agei[, .(yearBirth = min(yearBirth)), TOKEN_PERSON_ID]
d <- agei[d, on = 'TOKEN_PERSON_ID']
d[, age_impute := year - yearBirth]
d[, STARTAGE := fifelse(is.na(STARTAGE), age_impute, STARTAGE)]
rm(agei)
d[, age_impute := NULL]

# ===========================
# exclude ineligible episodes
# ---------------------------

d <- d[year != 2022]
d <- d[EPIORDER == 1]
d <- d[EPISTAT == 3]
d <- d[!is.na(STARTAGE)]
d <- d[region != 'Not England']

# ==================
# exclusion criteria
# ------------------

nrow(d) # 97549
d[, .N, factor(findInterval(STARTAGE, c(0, 15, 65)), 1:3, c('0-14', '15-64', '65+'))]
# 0-14 = 10; 65+ = 444
d <- d[STARTAGE >= 15 & STARTAGE <= 64]
nrow(d) # 97095
d[, .N, epo]
# planned = 3716; other = 1076
d <- d[epo == 'emergency']
nrow(d) # 96574

d <- droplevels(d)

# ==========
# age groups
# ----------

age_lims <- c(15, 25, 35, 45, 55)
d[, age_group := findInterval(STARTAGE, age_lims)]
d[, age_group := factor(age_group, seq_along(age_lims), age_lims)]

age_lims2 <- c(0, 18, seq(20, 70, 5))
d[, age_group2 := findInterval(STARTAGE, age_lims2)]
d[, age_group2 := factor(age_group2, seq_along(age_lims2), age_lims2)]
detailed_age_groups <- d[, .N, c('year', 'age_group2')]
detailed_age_groups[, N := pmax(N, 10)]
detailed_age_groups <- detailed_age_groups[order(year, age_group2)]
fwrite(detailed_age_groups, 'irid_age_group_by_year_22feb2023.csv')

# ================================
# further clinical characteristics
# --------------------------------

# operation
d[, operat := OPERSTAT == '1']

# readmissions
r <- r[ADMIMETH %in% c(21:25, '2A', '2B', '2C', '2D', 28)]
r <- r[TOKEN_PERSON_ID %chin% d$TOKEN_PERSON_ID]
r[, EPIKEY := NULL]
rc <- d[, .(TOKEN_PERSON_ID = TOKEN_PERSON_ID, EPIKEY = EPIKEY, index = ADMIDATE)][r, on = 'TOKEN_PERSON_ID', allow.cartesian = T]
rc[, dif := ADMIDATE - index]
d[, read := EPIKEY %in% rc[dif > 0 & dif < 29, unique(EPIKEY)]]

# duration of admission
d[, dur := as.integer(DISDATE - ADMIDATE) + 1L]
d$dur[d$dur < 0] <- NA_integer_

# =============
# age quantiles
# -------------

age_quantiles <- rbind(aggregate(d$STARTAGE, by = list(region = d$region, year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)),
                       cbind(region = 'England', aggregate(d$STARTAGE, by = list(year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))
age_quantiles <- data.table(age_quantiles)
names(age_quantiles) <- c('region', 'year', 'q0.05', 'q0.25', 'q0.5', 'q0.75', 'q0.95')
fwrite(age_quantiles, 'age_quantiles_22feb2023.csv')

age_quantiles[region == 'England'  & year %in% c(2002, 2021)]

# ============
# homelessness
# ------------

d[, homeless := EPIKEY %in% g[DiagCode == 'Z590', unique(EPIKEY)]]
d[, homeless := homeless == T | region == 'No fixed abode']

homelessness <- dcast(d, year ~ homeless, value.var = 'EPIKEY', fun.aggregate = length)
names(homelessness) <- c('year', 'no', 'yes')
homelessness[, total := yes + no]
homelessness[, prop := yes / total]
fwrite(homelessness, 'homeless_22feb2023.csv')

# ==================
# annual time trends
# ------------------

yrAge <- d[year >= 1998 & year <= 2022, .(admissions = .N), c('year', 'age_group')]
# redact small cells (very few of these)
yrAge[, admissions := pmax(10, admissions)]
fwrite(yrAge, 'admissions_by_year_and_age_group_22feb2023.csv')

# ============================
# table 1 and descriptive data
# ----------------------------

# age histogram

age_hist <- d[, .(n = pmax(10, .N)), STARTAGE][order(STARTAGE)]
fwrite(age_hist, 'age_histogram_22feb2023.csv')

# table 1

cv2 <- function (v = 'finalDis', dat = d, digs = 1) {
  a <- dat[, .(n = .N), v]
  a$pc <- a$n / sum(a$n)
  a$n <- formatC(a$n, big.mark = ',')
  a$pc <- format(round(a$pc * 100, digs), nsmall = digs, digits = digs)
  a$val <- paste0(a$n, '(', a$pc, ')')
  a$val <- gsub(' ', '', a$val)
  a$val <- gsub('\\(', ' (', a$val)
  a <- data.frame(variable = rep(v, nrow(a)), level = a[, get(v)], val = a$val)
  a[order(a$level),]
}

nv2 <- function (v, dat = d, digs = 1) {
  a <- quantile(dat[, get(v)], probs = c(0.5, 0.25, 0.75), na.rm = T)
  b <- c(mean(dat[, get(v)], na.rm = T), sd(dat[, get(v)], na.rm = T))
  a <- paste0(a[1], '[', a[2], '-', a[3], ']')
  b <- format(round(b, digs), nsmall = digs, digits = digs)
  b <- paste0(b[1], '[', b[2], ']')
  a <- gsub(' ', '', a)
  b <- gsub(' ', '', b)
  a <- gsub('\\[', ' [', a)
  b <- gsub('\\[', ' [', b)
  data.frame(variable = c(v, v), level = c('Median [IQR]', 'Mean [sd]'), val = c(a, b))
}

vars <- c('total2', 'age_group', 'STARTAGE', 'SEX', 'ETHNOS', 'imd5', 'homeless', 'region', 'finalDis', 'diagnosis', 'dur', 'operat', 'read')
d[, invasive := diagnosis %in% c('endocarditis', 'osteo', 'septicaemia', 'nf')]

def <- function (v, dat = d) if (class(d[, get(v)]) %in% c('integer', 'numeric')) nv2(v, dat = dat) else cv2(v, dat = dat)

table1 <- do.call(rbind, lapply(vars, def))

table1_by_diag <- lapply(c(total = list(d), invasive = list(d[invasive == T]), sstvi = list(d[invasive == F]), split(d, f = d$diagnosis)), function (x) {
  lapply(vars, def, dat = x)})
table1_by_diag <- lapply(table1_by_diag, function (x) do.call(rbind, x))
for (i in seq_along(table1_by_diag)) {
  names(table1_by_diag[[i]])[3] <- names(table1_by_diag)[i]
}
table1_by_diag <- Reduce(function(...) merge(..., all = TRUE, sort = F), table1_by_diag)

table1_by_year <- lapply(c(total = list(d), split(d, f = d$year)), function (x) {
  lapply(vars, def, dat = x)})
table1_by_year <- lapply(table1_by_year, function (x) do.call(rbind, x))
for (i in seq_along(table1_by_year)) {
  names(table1_by_year[[i]])[3] <- names(table1_by_year)[i]
}
table1_by_year <- Reduce(function(...) merge(..., all = TRUE, sort = F), table1_by_year)

fwrite(table1, 'table1_22feb2023.csv')
fwrite(table1_by_diag, 'table1_by_diag_22feb2023.csv')
fwrite(table1_by_year, 'table1_by_year_22feb2023.csv')

# ==================================================
# clinical characteristics before and after COVID-19
# --------------------------------------------------

covidStart <- as.Date('2020-03-23', origin = '1970-01-01')
d[, covid1 := 'before']
d$covid1[as.integer(d$ADMIDATE) %between%  c(covidStart, covidStart + 364)] <- 'after'
d$covid1[as.integer(d$ADMIDATE) < (covidStart - 365) | as.integer(d$ADMIDATE) > (covidStart + 364)] <- NA_character_
d[, covid1 := factor(covid1, c('before', 'after'))]
d[, died := finalDis == 'died']
d[, invasive := diagnosis %in% c('endocarditis', 'septicaemia', 'osteo', 'nf')]
d[, dummy := 1]

nv1 <- function (var, dat) {
  iqr <- with(dat[!is.na(covid1)], sapply(split(get(var), covid1), quantile, probs = c(0.5, 0.25, 0.75), na.rm = T))
  iqr <- paste0(iqr[1,], '(', iqr[2,], '-', iqr[3,], ')')
  iqr <- gsub('\\(', ' (', gsub(' ', '', iqr))
  p_value <- wilcox.test(get(var) ~ covid1, data = dat)$p.value
  p_value <- if(p_value < 0.001) '<0.001' else format(round(p_value, 3), nsmall = 3, digits = 3)
  data.table(t(c(var = var, mes = 'median (IQR)', `names<-`(iqr, c('before', 'after')), p_value = p_value)))
}

meansd <- function (x) c(mean(x, na.rm = T), sd(x, na.rm = T))

nv2 <- function (var, dat, digs = 1) {
  msd <- with(dat[!is.na(covid1)], sapply(split(get(var), covid1), meansd))
  msd <- format(round(msd, digs), nsmall = digs, digits = digs)
  msd <- paste0(msd[1,], '(', msd[2,], ')')
  msd <- gsub('\\(', ' (', gsub(' ', '', msd))
  p_value <- t.test(get(var) ~ covid1, data = dat)$p.value
  p_value <- if(p_value < 0.001) '<0.001' else format(round(p_value, 3), nsmall = 3, digits = 3)
  data.table(t(c(var = var, mes = 'mean (sd)', `names<-`(msd, c('before', 'after')), p_value = p_value)))
}

cv1 <- function (mes, dat, digs = 1) {
  a <- dcast(dat[!is.na(covid1)], get(mes) ~ covid1, value.var = 'TOKEN_PERSON_ID', fun.aggregate = length)
  p_value <- chisq.test(as.matrix(a[, -1]))$p.value
  p_value <- if(p_value < 0.001) '<0.001' else format(round(p_value, 3), nsmall = 3, digits = 3)
  a[, before_pc := format(round(before / sum(before) * 100, digs), nsmall = digs, digits = digs)]
  a[, after_pc := format(round(after / sum(after) * 100, digs), nsmall = digs, digits = digs)]
  a[, before := formatC(before, big.mark = ',')]
  a[, after := formatC(after, big.mark = ',')]
  a[, before := paste0(before, '(', before_pc, ')')]
  a[, after := paste0(after, '(', after_pc, ')')]
  a[, before := gsub('\\(', ' (', gsub(' ', '', before))]
  a[, after := gsub('\\(', ' (', gsub(' ', '', after))]
  a[, p_value := c(p_value, rep('', nrow(a)-1))]
  cbind(var = c(mes, rep('', nrow(a)-1)), a[, c('mes', 'before', 'after', 'p_value')])
}

covid_clinical <- rbind(cv1('dummy', dat = d),
                        nv2('dur', dat = d),
                        nv1('dur', dat = d),
                        cv1('died', dat = d),
                        cv1('invasive', dat = d))

fwrite(covid_clinical, 'covid_clinical_22feb2023.csv')

# ====================
# time series analysis
# --------------------


# make datasets
# -------------

# irid

dd <- dcast(d, ADMIDATE ~ diagnosis, value.var = 'total', fun.aggregate = length)
dd[, sstvi := abscess + cellulitis + phlebitis + other_ssti]
dd[, invasive := endocarditis + septicaemia + osteo + nf]
dd[, irid := sstvi + invasive]

# comparison admissions

file_names <- c(cocci = 'comparison_skin_18feb2023.csv',
                m1 = 'comparison_m1_18feb2023.csv',
                m2 = 'comparison_m2_18feb2023.csv',
                f11 = 'comparison_f11_21feb2023.csv')

nt <- mapply(fread, 
             file = file_names,
             SIMPLIFY = F)
for(i in seq_along(nt)) {
  nt[[i]]$ADMIDATE <- format_date(nt[[i]]$ADMIDATE)
  nt[[i]] <- nt[[i]][, .(n = sum(admissions)), ADMIDATE]
  names(nt[[i]])[2] <- names(file_names)[i]
}
nt <- Reduce(function(...) merge(..., all = TRUE), nt)
nt <- nt[year(ADMIDATE) %between% c(2002, 2022)]
all_days <- data.table(ADMIDATE = seq(from = as.Date('2002-01-01', origin = '1970-01-01'), 
                                      to = as.Date('2021-12-31', origin = '1970-01-01'),
                                      by = 'day'))
nt <- nt[all_days, on = 'ADMIDATE']

# combine data
# ------------

nt <- merge(nt, dd, by = 'ADMIDATE', all = T)
nt[is.na(nt)] <- 0L

# time variables

nt[, time := .I]
nt[, covid := ADMIDATE >= covidStart]
nt[, slope := .I]
nt[, slope := slope - (which(ADMIDATE == covidStart) - 1)]
nt$slope[nt$ADMIDATE < covidStart] <- 0L
nt[, yd := yday(ADMIDATE)]
nt[, year := year(ADMIDATE)]
nt <- nt[, .(daysInYear = max(yd)), year][nt, on = 'year']
nt[, s1 := yd / daysInYear]

# do time series analysis
# -----------------------

covid_newdata <- nt[covid == T, c('time', 's1')]
covid_newdata[, covid := F]
covid_newdata[, slope := 0]

tsf2 <- function (outcome = 'irid', B = 10, critval = qnorm(0.975), year_range = 2015:2021, harm = 2) {
  dat <- nt[year %in% year_range]
  f <- as.formula(paste0(outcome, '~ time + covid + slope + harmonic(s1,', harm, ', period = 1)'))
  m <- glm(f, data = dat, family = 'poisson')
  p <- predict(m, newdata = dat, se.fit = T)
  fi <- m$family$linkinv
  fit <- fi(p$fit)
  upper <- fi(p$fit + critval * p$se.fit)
  lower <- fi(p$fit - critval * p$se.fit)
  p_covid <- predict(m, newdata = covid_newdata, se.fit = T)
  fit_covid <- fi(p_covid$fit)
  upper_covid <- fi(p_covid$fit + critval * p_covid$se.fit)
  lower_covid <- fi(p_covid$fit - critval * p_covid$se.fit)
  plot(dat$time, fit, type = 'l', ylim = c(0, max(dat[, get(outcome)])))
  points(dat$time, dat[, get(outcome)], pch = 4, cex = 0.3, col = 'grey')
  lines(dat$time, upper)
  lines(dat$time, lower)
  lines(covid_newdata$time, fit_covid, lty = 3)
  lines(covid_newdata$time, upper_covid, lty = 3)
  lines(covid_newdata$time, lower_covid, lty = 3)
  covid <- c(coef(m)[names(coef(m)) == 'covidTRUE'], confint(m, parm = 'covidTRUE'))
  covid <- exp(covid)
  season_midpoint <- predict(m, newdata = data.frame(time = 7306, s1 = (1:365)/365, covid = F, slope = 0), type = 'response')
  mid <- min(season_midpoint) + diff(range(season_midpoint))/2
  season_midpoint <- which.min(abs(season_midpoint - mid)) / 365
  nd <- cbind(dat[, c('time', 'covid', 'slope')], s1 = season_midpoint)
  p2 <- predict(m, newdata = nd, se.fit = T)
  nd$fit <- m$family$linkinv(p2$fit)
  nd$upper <- m$family$linkinv(p2$fit + critval * p2$se.fit)
  nd$lower <- m$family$linkinv(p2$fit - critval * p2$se.fit)
  Bs <- sapply(seq_len(B), function (x) {
    if(x %% 10 == 0) print(x)
    Bn <- rpois(nrow(dat), dat[, get(outcome)])
    mB <- glm(Bn ~ time + covid + slope + harmonic(s1, 2, period = 1), data = dat, family = 'poisson')
    x <- predict(mB, newdata = data.frame(time = 7306, covid = F, s1 = (1:365)/365, slope = 0), type = 'response')
    c(ratio = max(x) / min(x), min = which.min(x), max = which.max(x))
  })
  return(list(
    fit = data.frame(fit = fit, lower = lower, upper = upper),
    cov = covid,
    rat = Bs,
    des = nd, # deseasonalised
    co2 = data.table(time = covid_newdata$time,
                     fit = fit_covid,
                     lower = lower_covid,
                     upper = upper_covid)
  ))
}

outcomes <- c('irid', 'cocci', 'm2', 'm1', 'invasive', 'f11')
set.seed(388)
sr <- lapply(outcomes, tsf2, B = 1000)
names(sr) <- outcomes

# time series plots
# -----------------

cols <- brewer.pal(8, 'Paired')[c(1:4, 7:8)]
x <- 'irid'
jan <- nt[yd == 1, time]
covid_day <- nt[ADMIDATE == '2020-03-23', time]

pf2 <- function (x, title = NA, counter = T, deseason = T, year_range = 2015:2021, ci = F) {
  jan2 <- nt[year %in% year_range & yd == 1, time]
  ymax <- max(nt[year %in% year_range, get(x)]) * 1.05
  xmin <- nt[year %in% year_range, min(time)]
  xmax <- nt[year %in% year_range, max(time)] + 1
  plot(1, type = 'n', xlim = c(xmin, xmax), ylim = c(0, ymax), axes = F, xlab = NA, ylab = NA)
  rect(xmin, 0, xmax, ymax, col = 'white')
  segments(jan2, 0, y1 = ymax, lwd = 0.3, col = 'grey70')
  rect(xmin, 0, xmax, ymax)
  points(nt[year %in% year_range, time], nt[year %in% year_range, get(x)], pch = 4, cex = 0.6, col = 'grey80')
  if (deseason) {
    with(sr[[x]]$des, {
      if (ci) polygon(x = c(time, rev(time)), y = c(lower, rev(upper)), col = cols[5], border = NA)
      lines(x = time, y = fit, col = cols[6])
    })}
  if (counter) {
    with(sr[[x]]$co2, {
      if (ci) polygon(x = c(time, rev(time)), y = c(lower, rev(upper)), col = cols[3], border = NA)
      lines(x = time, y = fit, col = cols[4])
    })}
  with(sr[[x]]$fit, {
    if (ci) polygon(x = c(nt[year %in% year_range, time], rev(nt[year %in% year_range, time])), y = c(lower, rev(upper)), col = cols[1], border = NA)
    lines(x = nt[year %in% year_range, time], y = fit, col = cols[2])
  })
  segments(covid_day, 0, y1 = ymax, lwd = 0.8, col = 'red', lty = 2)
  axis(2, at = yax(ymax), las = 2, pos = xmin)
  xtick <- unique(c(xmin, jan2))
  axis(1, at = c(xtick, xmax), labels = F, pos = 0)
  text(jan2 + diff(c(xtick, xmax) / 2), ymax * -0.05, nt[year %in% year_range & yd == 1, year], srt = 60, adj = 1)
  text(xmin, ymax * 1.1, title, cex = 1.05, adj = 0)
}

pf3 <- function (...) {
  m <- matrix(1:2, ncol = 2, byrow = T)
  layout(m, widths = c(2, 0.8))
  par(xpd = NA, mar = c(3, 2, 3, 2), oma = c(2, 3, 0, 0))
  pf2(...)
  par(mar = c(0, 0, 0, 0))
  plot(1, type = 'n', xlim = c(0, 10), ylim = c(0, 10), xlab = NA, ylab = NA, axes = F)
  ys <- seq(1.5, 8.5, length.out = 5)
  segments(0, ys[1:3], x1 = 3, col = cols[c(6, 4, 2)])
  text(3.5, ys[1:3], rev(c('Trend with seaonality\nand slope/step change\nat 23 March 2020', 'Counterfactual trend\nwith no change\nat 23 March 2020', 'Deseasonalised\ntrend')), adj = 0)
  segments(1.5, ys[4] - mean(diff(ys)/2), y1 = ys[4] + mean(diff(ys)/2), col = 'red', lty = 2)
  text(3.5, ys[4], '23 March 2020', adj = 0)
  points(1.5, ys[5], pch = 4, col = 'grey30', cex = 2)
  text(3.5, ys[5], 'Observed\ndaily count', adj = 0)
  mtext('Number of hospital admissions per day', side = 2, line = 1, outer = T)
}

tiff('Fig2.tiff', height = 8, width = 10, units = 'in', res = 600, family = 'Tahoma')
pf3('irid')
dev.off()

emf('FigSX_cocci.emf', height = 8, width = 10, family = 'Tahoma')
pf3('cocci')
dev.off()

emf('FigSX_invasive.emf', height = 8, width = 10, family = 'Tahoma')
pf3('invasive')
dev.off()

emf('FigSX_m1.emf', height = 8, width = 10, family = 'Tahoma')
pf3('m1')
dev.off()

emf('FigSX_m2.emf', height = 8, width = 10, family = 'Tahoma')
pf3('m2')
dev.off()

emf('FigSX_f11.emf', height = 8, width = 10, family = 'Tahoma')
pf3('f11')
dev.off()

# misc results
# ------------

# model coefficients

nt[, time2 := time / 365]
nt[, slope2 := slope / 365]
m <- glm(irid ~ time2 + covid + slope2 + harmonic(s1, 2, period = 1), data = nt[year %in% 2015:2021], family = 'poisson')
m_res <- cbind(coef(m), confint(m))
m_res <- exp(m_res)
m_res <- format(round(m_res, 3), digits = 3, nsmall = 3)
m_res <- data.table(var = row.names(m_res), IRR = paste0(m_res[,1], ' (', m_res[,2], '-', m_res[,3], ')'))
fwrite(m_res, 'model_coefficients_22feb2023.csv')

# average daily numbers per year

nt[, dummy := 1]
diagnoses <- c('irid', unique(as.character(d$diagnosis)))
yearly <- nt[, lapply(.SD, function (x) sum(x)), c('year', 'covid'), .SD = c(diagnoses, 'dummy')]
yearly_rates <- mapply(vpt, x = yearly[, diagnoses, with = F], t = list(yearly$dummy), SIMPLIFY = F)
yearly_rates <- lapply(yearly_rates, function (x) `dimnames<-`(t(x), value = list(paste0(yearly$year, '-', yearly$covid), c('point', 'lower', 'upper'))))
yearly_rates$irid

# table 2

table2 <- lapply(sr, function (x) apply(x$rat, 1, quantile, probs = c(0.5, 0.025, 0.975)))
dys <- seq(from = as.Date('2022-01-01'), to = as.Date('2022-12-31'), by = 'day')
min_max <- lapply(table2, function (x) matrix(format(dys[x[,2:3]], '%d %b'), ncol = 2))
min_max <- `colnames<-`(t(sapply(min_max, function (x) x[1,])), c('low', 'high'))
ratios <- lapply(table2, function (x) format(round(x[, 1], 2), digits = 2, nsmall = 2))
ratios <- sapply(ratios, function (x) paste0(x[1], ' (', x[2], ', ', x[3], ')'))
covid_pc <- (1 - sapply(sr, function (x) x$cov)) * 100
covid_pc <- format(round(covid_pc, 1), digits = 1, nsmall = 1)
covid_pc <- paste0(covid_pc[1,], '(', covid_pc[2,], ',', covid_pc[3,], ')')
covid_pc <- gsub(',', ', ', gsub('\\(', ' (', gsub(' ', '', covid_pc)))
table2 <- cbind(var = names(sr), ratio = ratios, min_max, covid = covid_pc)
fwrite(table2, 'table2_22feb2023.csv')

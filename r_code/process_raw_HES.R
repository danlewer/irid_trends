library(data.table)
library(lubridate)
library(stringi)

# secure folder location
setwd("Z:/IA_UserData/dan.lewer/pwid_bacterial_infections/update_2022")

format_date <- function (x) {
  a <- suppressWarnings(cbind(dmy(x), ymd(x)))
  a <- a[cbind(seq_len(nrow(a)), max.col(!is.na(a)))]
  as.Date(a, origin = '1970-01-01')
}

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

icd10 <- list(
  abscess = 'L02',
  cellulitis = 'L03',
  other_ssti = c('A48', 'L088', 'L089', 'L97', 'L984', 'L988', 'L989', 'R02'),
  phlebitis = 'I80',
  endocarditis = c('B376', 'I330', 'I339', 'I38', 'I39'),
  septicaemia = c('A40', 'A41', 'R572', 'B377'),
  osteo = c('M86', 'M00', 'M465'),
  nf = 'M762'
)

# ===========================
# read patient-level HES data
# ---------------------------

d_raw <- fread("irid_hes_extract_12oct2022.csv")
d <- copy(d_raw)

# ================
# format variables
# ----------------

# age, sex, ethnicity
d[, STARTAGE := as.integer(STARTAGE)]
age_lims <- c(15, 25, 35, 45, 54)
d[, age_group := findInterval(STARTAGE, age_lims)]
d[, age_group := factor(age_group, seq_along(age_lims), age_lims)]
d[, SEX := factor(SEX, c(1, 2, 9, 0), c('male', 'female or not known', 'female or not known', 'female or not known'))]
d[, ETHNOS := gsub(" ", "", ETHNOS)]
d$ETHNOS[d$ETHNOS %in% c('A', 'B', 'C', '0')] <- 'White'
d$ETHNOS[d$ETHNOS %in% c('D', 'E', 'F', 'G')] <- 'Mixed'
d$ETHNOS[d$ETHNOS %in% c('H', 'J', 'K', 'L', 4:6)] <- 'Asian'
d$ETHNOS[d$ETHNOS %in% c('M', 'N', 'P', 1:3)] <- 'Black'
d$ETHNOS[d$ETHNOS %in% c('R', 'S', 'Z', 'X', 7:9, 99)] <- 'Other or unknown'
d[, ETHNOS := factor(ETHNOS, c('White', 'Black', 'Asian', 'Mixed', 'Other or unknown'))]

# region
d[, region1 := factor(RESGOR, c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'U', 'Y', 'NULL', 'S', 'W', 'X', 'Z'), c('North East', 'North West', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East', 'South West', 'No fixed abode', 'Unknown', 'Unknown', 'Not England', 'Not England', 'Not England', 'Not England'))]
d[, region2 := factor(RESGOR_ONS, c('E12000001', 'E12000002', 'E12000003', 'E12000004', 'E12000005', 'E12000006', 'E12000007', 'E12000008', 'E12000009', 'M99999999', 'NULL', 'U', 'W99999999', 'X', 'Y'), c('North East', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East', 'South West', 'Not England', 'Unknown', 'No fixed abode', 'Not England', 'Not England', 'Unknown'))]
d[, region := fifelse(region1 == 'Unknown', as.character(region2), as.character(region1))]

# imd
d[, IMD04RK := as.numeric(d$IMD04RK)]
d[, imd5 := findInterval(IMD04RK, seq(0, 32482, length.out = 6))]
d$imd5[is.na(d$imd5)] <- 'missing'
d[, imd5 := factor(imd5, c(1:5, 'missing'), c('1 - most deprived', 2:4, '5 - least deprived', 'missing'))]

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
d <- d[year >= 2002 & year <= 2021] # TOKEN_PERSON_ID missing before 2001
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

# ===============
# impute STARTAGE
# ---------------

d[, yearBirth := year - STARTAGE]
d$yearBirth[d$yearBirth < 0] <- NA_integer_
yearBirth <- d[, .(yearBirth = round(mean(yearBirth, na.rm = T), 0)), TOKEN_PERSON_ID]
d[, yearBirth := NULL]
d <- yearBirth[d, on = 'TOKEN_PERSON_ID']
d[is.na(STARTAGE), table(is.na(yearBirth))] # doesn't really help

# ===========================
# exclude ineligible episodes
# ---------------------------

d <- d[EPIORDER == 1]
d <- d[EPISTAT == 3]
d <- d[!is.na(STARTAGE)]
d <- d[region != 'Not England']

# ==================
# exclusion criteria
# ------------------

nrow(d) # 97576
d[, .N, factor(findInterval(STARTAGE, c(0, 15, 65)), 1:3, c('0-14', '15-64', '65+'))]
# 0-14 = 10; 65+ = 443
d <- d[STARTAGE >= 15 & STARTAGE <= 64]
nrow(d) # 97123
d[, .N, epo]
# planned = 3772; other = 1048
d <- d[epo == 'emergency']
nrow(d) # 92303

d <- droplevels(d)

# =================
# check seasonality
# -----------------

d[, month := month(ADMIDATE, label = T)]
season <- d[year >= 2012, .N, c('month', 'year')]
season[, month := factor(as.character(month), month.abb)]
season[, covid := year >= 2020]
season$covid[season$month %in% c('Jan', 'Feb') & season$year == 2020] <- F
season <- season[order(year, month)]
season[, x := .I]
season_model <- glm(N ~ month + x + covid, data = season, family = 'poisson')
season_model2 <- glm(N ~  x + covid, data = season, family = 'poisson')
season[, p := predict(season_model, newdata = season, type = 'response')]
season[, p2 := predict(season_model2, newdata = season, type = 'response')]
season[, r1 := p - N]
season[, r2 := p2 - N]

plot(season$x, season$N, ylim = c(0, max(season$N)))
lines(season$x, season$p)
lines(season$x, season$p2)

av_month <- season[, mean(N), month]
plot(1, type = 'n', xlim = c(0, 12), ylim = c(0, max(av_month$V1)), axes = F, xlab = NA, ylab = NA)
with(av_month, rect(0:11, 0, 1:12, V1))
axis(1, 0:11 + 0.5, av_month$month, las = 2, pos = 0, tick = F)
axis(2, pos = 0, las = 2)

# ===================
# detailed age groups
# -------------------

age_lims2 <- c(0, 18, seq(20, 70, 5))
d[, age_group2 := findInterval(STARTAGE, age_lims2)]
d[, age_group2 := factor(age_group2, seq_along(age_lims2), age_lims2)]
detailed_age_groups <- d[, .N, c('year', 'age_group2')]
detailed_age_groups[, N := pmax(N, 10)]
fwrite(detailed_age_groups, 'irid_age_group_by_year_12oct2022.csv')

# =========================================
# check reduction in March 2020 by pt group
# -----------------------------------------

abg <- Reduce(merge, lapply(c('total', 'age_group', 'region', 'diagnosis', 'SEX'), function (x) dcast(d, admiMonth ~ get(x), value.var = 'total', fun.aggregate = length)))

par(mfrow = c(5, 6), mar = c(1, 0, 2, 0), oma = c(3, 3, 3, 3))
lapply(names(abg)[-1], function (x) {
  pd <- abg[stri_sub(admiMonth, 5, 8) > 2018]
  plot(pd[, get(x)], type = 'b', ylim = c(0, max(pd[, get(x)])), axes = F, xlab = NA, ylab = NA, col = 'red')
  # axis(1, seq_along(pd$admiMonth), pd$admiMonth, las = 2)
  axis(1, seq_along(pd$admiMonth), labels = F)
  axis(2, las = 2)
  abline(v = which(pd[, admiMonth] == 'Mar 2020'))
  title(main = x)
}) # reduction around March 2020 appears across age groups, regions, diagnoses, and sex

# ============================
# table 1 and descriptive data
# ----------------------------

# age histogram

age_hist <- d[, .(n = pmax(10, .N)), STARTAGE][order(STARTAGE)]
fwrite(age_hist, 'age_histogram_12oct2022.csv')

cv <- function (v) {
  a <- d[, table(get(v))]
  b <- d[, table(get(v), period)]
  a <- cbind(total = a, b)
  tot <- colSums(a)
  p <- t(t(a) / tot)
  p <- format(round(p * 100, 1), nsmall = 1, digits = 1)
  a <- formatC(a, big.mark = ',')
  r <- paste0(a, '(', p, ')')
  r <- gsub("\\(", " (", gsub(" ", "", r))
  r <- matrix(r, ncol = ncol(a), dimnames = list(row.names(a), colnames(a)))
  cbind(variable = c(v, rep('', nrow(r)-1)), level = row.names(r), r)
}

nv <- function (v) {
  a <- with(d, tapply(get(v), period, quantile, probs = c(0.5, 0.25, 0.75), na.rm = T))
  a <- sapply(a, function (x) paste0(x[1], ' [', x[2], '-', x[3], ']'))
  b <- quantile(d[, get(v)], probs = c(0.5, 0.25, 0.75), na.rm = T)
  b <- paste0(b[1], ' [', b[2], '-', b[3], ']')
  c(variable = v, level = 'Median [IQR]', total = b, a)
}

table1 <- rbind(cv('total'), 
                cv('age_group'), 
                nv('STARTAGE'), 
                cv('SEX'), 
                cv('ETHNOS'), 
                cv('imd5'), 
                cv('region'),
                cv('finalDis'),
                cv('diagnosis'),
                cv('period'))

fwrite(table1[,1:3], 'table1_12oct2022.csv')

# =============
# age quantiles
# -------------

age_quantiles <- rbind(aggregate(d$STARTAGE, by = list(region = d$region, year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      cbind(region = 'England', aggregate(d$STARTAGE, by = list(year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))
age_quantiles <- data.table(age_quantiles)
age_quantiles <- age_quantiles[year %in% 1998:2021]
names(age_quantiles) <- c('region', 'year', 'q0.05', 'q0.25', 'q0.5', 'q0.75', 'q0.95')
fwrite(age_quantiles, 'age_quantiles_12oct2022.csv')

# ===========
# time trends
# -----------

yrAge <- d[year >= 1998 & year <= 2021, .(admissions = .N), c('year', 'age_group')]
# redact small cells (very few of these)
yrAge[, admissions := pmax(10, admissions)]
fwrite(yrAge, 'admissions_by_year_and_age_group_12oct2022.csv')

# ==================================
# severity before and after COVID-19
# ----------------------------------

d[, dur := as.integer(DISDATE - ADMIDATE)]
d$dur[d$dur < 0] <- NA_integer_
covidStart <- as.Date('2020-03-23', origin = '1970-01-01')
d[, covid1 := 'before']
d$covid1[as.integer(d$ADMIDATE) %between%  c(covidStart, covidStart + 364)] <- 'after'
d$covid1[as.integer(d$ADMIDATE) < (covidStart - 365) | as.integer(d$ADMIDATE) > (covidStart + 364)] <- NA_character_

d[, .(mn = mean(dur, na.rm = T), sd = sd(dur, na.rm = T)), c('covid1', 'diagnosis')]
d <- droplevels(d)

durs <- dcast(d[!is.na(covid1)], diagnosis ~ covid1, value.var = 'dur', fun.aggregate = cbind(mean, sd), na.rm = T)

ttests <- sapply(split(d[!is.na(covid1)], f = d$diagnosis[!is.na(d$covid1)]), function (x) with(x, t.test(dur ~ covid1)$p.value))
mwut <- sapply(split(d[!is.na(covid1)], f = d$diagnosis[!is.na(d$covid1)]), function (x) with(x, wilcox.test(dur ~ covid1)$p.value))


durs <- data.table(diagnosis = names(ttests), ttests = ttests)[durs, on = 'diagnosis']
durs <- data.table(diagnosis = names(mwut), mwut = mwut)[durs, on = 'diagnosis']
setnames(durs, c('dur_fun1_before', 'dur_fun1_after', 'dur_fun2_before', 'dur_fun2_after'), c('mn_before', 'mn_after', 'sd_before', 'sd_after'))
durs[, c('mn_before', 'mn_after', 'sd_before', 'sd_after') := lapply(.SD, function (x) format(round(x, 2), digits = 2, nsmall = 2)), .SDcols = c('mn_before', 'mn_after', 'sd_before', 'sd_after')]
durs[, before := paste0(mn_before, '(', sd_before, ')')]
durs[, before := gsub(' ', '', before)]
durs[, before := gsub('\\(', ' (', before)]
durs[, after := paste0(mn_after, '(', sd_after, ')')]
durs[, after := gsub(' ', '', after)]
durs[, after := gsub('\\(', ' (', after)]
durs[, pvalue := ifelse(mwut < 0.001, '<0.001', format(round(mwut, 3), digits = 3, nsmall = 3))]

dur_counts <- dcast(d[!is.na(covid1)], diagnosis ~ covid1, value.var = 'adID', fun.aggregate = length)
setnames(dur_counts, c('before', 'after'), c('n_before', 'n_after'))
durs <- dur_counts[durs, on = 'diagnosis']
durs <- durs[, c('diagnosis', 'n_before', 'n_after', 'before', 'after', 'pvalue')]
fwrite(durs, 'los_before_after_covid_14oct2022.csv')

# =======================
# seasonal trends in IRID
# -----------------------

d[, mth := month(ADMIDATE, label = T)]
d[, mth := as.character(mth)]  # otherwise it's ordered
d[, mth := factor(mth, month.abb)]

dx <- d[, unique(as.character(diagnosis))]

seasonalIRID <- dcast(d, year + mth ~ diagnosis, value.var = 'TOKEN_PERSON_ID', fun.aggregate = length)
seasonalIRID <- seasonalIRID[order(year, mth)]
seasonalIRID[, irid := rowSums(seasonalIRID[, dx, with = F])]

#  =====================================
#  comparison trends in other infections
#  -------------------------------------

nt_names <- list(all_staph = 'emergency_admissions_all_staph_12oct2022.csv',
                 skin_staph = 'emergency_admissions_skin_staph_12oct2022.csv',
                 all_strep = 'emergency_admissions_all_strep_12oct2022.csv',
                 skin_strep = 'emergency_admissions_skin_strep_12oct2022.csv',
                 tcodes = 'emergency_admissions_tcodes_12oct2022.csv',
                 f11 = 'emergency_admissions_f11_12oct2022.csv')
nt <- lapply(nt_names, fread)
nt <- lapply(nt, `names<-`, c('ADMIDATE', 'admissions'))
nt <- lapply(nt, function (x) {
  x$d <- format_date(x$ADMIDATE)
  x$year <- year(x$d)
  x$mth <- month(x$d)
  x[!is.na(d), .(a = pmax(10, sum(admissions))), c('year', 'mth')]
})
for (i in seq_along(nt)) {
  names(nt[[i]])[3] <- names(nt_names)[i]
}
nt <- Reduce(function(...) merge(..., all = TRUE), nt)
nt <- nt[year >= 2012 & year <= 2021]
# calculate non-skin straph and strep
nt[, other_staph := all_staph - skin_staph]
nt[, other_strep := all_strep - skin_strep]
nt[, mth := factor(mth, 1:12, month.abb)]

#  ===================
#  drug-related deaths
#  -------------------

ons <- fread("H:/irid_time_trends_HES_2022/ons_daily_drd.csv")
ons[, date := dmy(date)]
ons[, year := year(date)]
ons <- ons[year >= 2012]
ons[, opioid := as.integer(opioid)]
ons[, mth := month(date)]
ons[, mth := factor(mth, 1:12, month.abb)]

# impute opioid values

ons <- ons[!is.na(opioid), .(prop_opioid = sum(opioid) / sum(all)), year][ons, on = 'year']
ons[, opioid2 := fifelse(is.na(opioid), round(prop_opioid * all, 0), opioid)]
ons <- ons[, .(all_drd = sum(all), opioid_drd = sum(opioid2)), c('year', 'mth')]
ons[, non_opioid_drd := all_drd - opioid_drd]

#  ====
#  join
#  ----

ss <- seasonalIRID[nt, on = c('year', 'mth')]
ss <- ons[ss, on = c('year', 'mth')]

#  ==========
#  add labels
#  ----------

ss[, time := .I]
covidMth <- ss[year == 2020 & mth == 'Mar', time]
ss[, covid := time >= covidMth]
ss[, sstvi := abscess + cellulitis + other_ssti + phlebitis]
ss[, invasive := endocarditis + septicaemia + osteo]

# write

ss[, c('osteo', 'septicaemia', 'endocarditis') := NULL]
fwrite(ss, 'counts_per_month_14oct2022.csv')

# ======================
# local authority counts
# ----------------------

# add codes / names

la_lookup <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/la_lookup.csv'))
setDT(la_lookup)

# check mapping of LA09_code
d[, LA09_maps := RESLADST %in% la_lookup$LA09_CODE]
# U = no fixed abode; Y = not known
missing_codes <- d[LA09_maps == F & !(RESLADST %in% c('NULL', 'U', 'Y')), .N, RESLADST][order(N, decreasing = T)]
sum(missing_codes$N) / nrow(d) * 100 # about 3% do not map to LA09_CODE

la_lookup <- la_lookup[, c('GOR_NAME', 'LA09_CODE', 'LA09_NAME', 'LAD10CD')]
setnames(la_lookup, 'LA09_CODE', 'RESLADST')
la_lookup <- unique(la_lookup)
d <- la_lookup[d, on = 'RESLADST']

# population estimates for age/period standardised rates

pop <- read.csv(url('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/la_pop_estimates.csv'))
setDT(pop)
names(pop)[1:2] <- c('name', 'LA09_CODE')
pop$name <- NULL
pop <- melt(pop, id.vars = c('year', 'LA09_CODE'), variable.name = 'age', value.name = 'pop')
pop[, age := gsub('Age.|Aged.|\\.', '', age)]
pop[, age := as.integer(age)]
pop[, pop := as.integer(pop)]
pop[, age_group := findInterval(age, age_lims)]
pop[, age_group := factor(age_group, seq_along(age_lims), age_lims)]
# add 2021 (assume same as 2020)
pop2021 <- pop[year == 2020]
pop2021[, year := 2021]
pop <- rbind(pop, pop2021)
pop[, period := findInterval(year, year_lims)]
pop[, period := factor(period, seq_along(year_lims), year_lims)]
pop <- pop[stri_sub(LA09_CODE, 0, 1) == 'E' & age %in% 15:64, .(pop = sum(pop)), c('period', 'LA09_CODE', 'age_group')]
setnames(pop, 'LA09_CODE', 'LAD10CD')

# admissions by LA, age, and period

la_count <- d[!is.na(LAD10CD) & year %in% 2002:2021, .N, c('LAD10CD', 'period', 'age_group')]
la_count <- pop[la_count, on = c('LAD10CD', 'period', 'age_group')]
la_count <- la_count[!is.na(pop)]

# reference population (age structure of whole country by period)

ref <- pop[, .(refpop = sum(pop)), c('period', 'age_group')]
la_count <- ref[la_count, on = c('period', 'age_group')]
la_count <- la_count[order(LAD10CD, period, age_group)]

# monte-carlo simulation of standardised rates

set.seed(23)
B <- 1000
simN <- matrix(rpois(nrow(la_count) * B, la_count$N), ncol = B)
simRate <- simN / la_count$pop
simE <- simRate * la_count$refpop
simNames <- paste0('sim', seq_len(B))
colnames(simE) <- simNames
la_count <- cbind(la_count, simE)
simSR_byPeriod <- la_count[, lapply(.SD, function (x) sum(x) / sum(refpop) * 1000000), .SD = simNames, c('period', 'LAD10CD')]
simSR_byPeriod <- cbind(simSR_byPeriod[, c('period', 'LAD10CD')], t(apply(as.matrix(simSR_byPeriod[, simNames, with = F]), 1, quantile, probs = c(0.5, 0.025, 0.975))))
simSR_allPeriod <- la_count[, lapply(.SD, function (x) sum(x) / sum(refpop) * 1000000), .SD = simNames, 'LAD10CD']
simSR_allPeriod <- cbind(simSR_allPeriod[, 'LAD10CD'], t(apply(as.matrix(simSR_allPeriod[, simNames, with = F]), 1, quantile, probs = c(0.5, 0.025, 0.975))))
simSR <- rbind(simSR_byPeriod, simSR_allPeriod, fill = T)

# point estimates

la_count[, (simNames) := NULL]
la_count[, rate := N / pop]
la_count[, e := rate * refpop]
simSR <- rbind(la_count[, .(sr = sum(e) / sum(refpop) * 1000000), c('period', 'LAD10CD')],
               la_count[, .(sr = sum(e) / sum(refpop) * 1000000), 'LAD10CD'], fill = T)[simSR, on = c('period', 'LAD10CD')]
simSR <- la_lookup[simSR, on = 'LAD10CD']

fwrite(simSR, 'standardised_rate_by_LA_14oct2022.csv')

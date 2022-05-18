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

d_raw <- fread("irid_hes_extract_17may2022.csv")

d <- copy(d_raw)

# ================
# format variables
# ----------------

# age, sex, ethnicity
d[, STARTAGE := as.integer(STARTAGE)]
age_lims <- c(15, 25, 34, 44, 54)
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
d[, region := factor(RESGOR, c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'J', 'K', 'U', 'Y', 'NULL', 'S', 'W', 'X', 'Z'), c('North East', 'North West', 'North West', 'Yorkshire & Humber', 'East Midlands', 'West Midlands', 'East of England', 'London', 'South East', 'South West', 'No fixed abode', 'Unknown', 'Unknown', 'Not England', 'Not England', 'Not England', 'Not England'))]

# imd
d[, IMD04RK := as.numeric(d$IMD04RK)]
d[, imd5 := findInterval(IMD04RK, seq(0, 32482, length.out = 6))]
d$imd5[is.na(d$imd5)] <- 'missing'
d[, imd5 := factor(imd5, c(1:5, 'missing'), c('1 - most deprived', 2:4, '5 - least deprived', 'missing'))]

# dates
date_cols <- c('ADMIDATE', 'DISDATE')
d[, (date_cols) := lapply(.SD, format_date), .SDcols = date_cols]
d[, LOS := as.integer(DISDATE - ADMIDATE) + 1] # this is more complete than SPELDUR
d$LOS[d$LOS < 0] <- NA_integer_
d[, SPELDUR := as.integer(SPELDUR)]
d[, LOS := fifelse(is.na(LOS), SPELDUR + 1L, LOS)]
LOS_lims <- c(1:5, 10)
d[, LOS_group := findInterval(LOS, LOS_lims)]
d$LOS_group[is.na(d$LOS_group)] <- 'missing'
d[, LOS_group := factor(LOS_group, c(seq_along(LOS_lims), 'missing'), c(LOS_lims, 'missing'))]
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
diagpos <- sapply(sapply(icd10, paste0, collapse = '|'), grepl, d$DIAG4_01)
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

nrow(d) # 98,240
d[, .N, factor(findInterval(STARTAGE, c(0, 15, 65)), 1:3, c('0-14', '15-64', '65+'))]
# 0-14 = 14; 65+ = 445
d <- d[STARTAGE >= 15 & STARTAGE <= 64]
nrow(d) # 97,781
d[, .N, epo]
# planned = 3806; other = 1079
d <- d[epo == 'emergency']
nrow(d) # 92896

d <- droplevels(d)

# ============================
# table 1 and descriptive data
# ----------------------------

# age histogram

age_hist <- d[, .(n = pmax(10, .N)), STARTAGE][order(STARTAGE)]
fwrite(age_hist, 'age_histogram_16may2022.csv')

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
                cv('LOS_group'),
                nv('LOS'),
                cv('finalDis'),
                cv('diagnosis'),
                cv('period'))

fwrite(table1, 'table1_16may2022.csv')

# =============
# age quantiles
# -------------

age_quantiles <- rbind(aggregate(d$STARTAGE, by = list(region = d$region, year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)),
      cbind(region = 'England', aggregate(d$STARTAGE, by = list(year = d$year), quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))
age_quantiles <- data.table(age_quantiles)
age_quantiles <- age_quantiles[year %in% 1998:2021]
names(age_quantiles) <- c('region', 'year', 'q0.05', 'q0.25', 'q0.5', 'q0.75', 'q0.95')
fwrite(age_quantiles, 'age_quantiles_16may2022.csv')

# ===========
# time trends
# -----------

yrAge <- d[year >= 1998 & year <= 2021, .(admissions = .N), c('year', 'age_group')]
# redact small cells (very few of these)
yrAge[, admissions := pmax(10, admissions)]
fwrite(yrAge, 'admissions_by_year_and_age_group_16may2022.csv')

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

fwrite(simSR, 'standardised_rate_by_LA_18may2022.csv')

# =================
# estimate of costs
# -----------------

sensitivity <- 0.528 # proportion of admissions captured
yr_diag <- d[, .N, c('year', 'diagnosis', 'region')]
yr_diag[, scaledN := round(N / sensitivity, 0)]
cost_per_admission <- data.table(diagnosis = c('abscess', 'cellulitis', 'phlebitis', 'septicaemia', 'osteo', 'endocarditis', 'other_ssti'),
                                 cost = c(4307, 3579, 3261, 8687, 14134, 12963, 4980))
yr_diag <- cost_per_admission[yr_diag, on = 'diagnosis']
yr_diag[, cost2 := scaledN * cost]
yr_diag[region == 'London', sum(scaledN), year][order(year)] # slightly lower than 2011 estimate based on rate of hospitalisation in SLAM cohort
yr_diag[year %in% 1998:2021, .(total_cost = sum(cost2)), year][order(year)]

#    year total_cost
# 1: 2002   32064274
# 2: 2003   42601445
# 3: 2004   39939118
# 4: 2005   35736869
# 5: 2006   35717421
# 6: 2007   34279994
# 7: 2008   35145964
# 8: 2009   30926505
# 9: 2010   31828843
# 10: 2011   28169732
# 11: 2012   30037153
# 12: 2013   34341237
# 13: 2014   40967363
# 14: 2015   47907460
# 15: 2016   55946814
# 16: 2017   64080143
# 17: 2018   69542125
# 18: 2019   69610960
# 19: 2020   51427388
# 20: 2021   45805940

#  ========
#  COVID-19
#  --------

covid19 <- d[year > 2017 & year < 2022, .N, admiMonth]
fwrite(covid19, 'monthly_from_jan2018_16may2022.csv')

#  =============================================
#  national trends for other types of admissions
#  ---------------------------------------------

fl <- list(
  all_cause = "emergency_admissions_per_day.csv",
  substances = "emergency_admissions_psychoactive_substances.csv", # F10-F19 - Psychoactive substance use
  alcohol = "emergency_admissions_alcohol.csv", # F10 - Alcohol
  appendix = "emergency_admissions_appendix.csv", # K35-K38 - Diseases of the appendix including acute apendicitis
  asthma = "emergency_admissions_asthma.csv", # J45 # Asthma
  diabetes = "emergency_admissions_diabetes.csv", # E10-E14 # Diabetes mellitus
  dorsopathies = "emergency_admissions_dorsopathies.csv", # M40-M54 # Dorsopathies
  episodic_paroxysmal = "emergency_admissions_episodic_paroxysmal.csv", # G40-G47 # Episodic and paroxysmal disorders, including epilepsy
  enteritis = "emergency_admissions_noninfective_enteritis.csv", #K50-K52 # Noninfective enteritis and colitis
  urinary = "emergency_admissions_urinary.csv" #N30-N39
)

ad <- lapply(fl, fread)
for(i in seq_along(ad)) {
  ad[[i]]$ADMIDATE <- format_date(ad[[i]]$ADMIDATE)
  ad[[i]]$admiMonth <- paste0(month(ad[[i]]$ADMIDATE, label = T, abbr = T), ' ', year(ad[[i]]$ADMIDATE))
  ad[[i]] <- ad[[i]][, sum(admissions), admiMonth]
  names(ad[[i]])[2] <- names(fl)[i]
}
ad <- Reduce(merge, ad)
ad[, admiMonth := factor(admiMonth, levels(d$admiMonth))]
ad <- ad[order(admiMonth)]
ad <- ad[!is.na(admiMonth)]
ad <- ad[stri_sub(admiMonth, 5, 9) %in% as.character(1998:2021)]

# check no small counts
sapply(ad[, names(fl), with = F], range)

fwrite(ad, 'monthly_trends_other_causes_16may2022.csv')

library(data.table)
library(extrafont)
loadfonts(device = "win")
library(devEMF)

propCI <- function(X, N, ..., form = T, digs = 0, pc = T) {
  r <- t(rbind(X/N, mapply(function(x, n) prop.test(x, n, ...)$conf.int[1:2], x = X, n = N)))
  colnames(r) <- c('prop', 'lower', 'upper')
  if (form == T) {
    if (pc == T) {r <- r * 100}
    r <- format(round(r, digs), nsmall = digs, digits = digs)
    r <- paste0(r[,1], '(', r[,2], '-', r[,3], ')')
    gsub('\\(', ' (', gsub(' ', '', r))
  } else {
    r
  }
}

d <- fread('https://raw.githubusercontent.com/danlewer/irid_trends/main/input_data/uam_ssti.csv', header = T)

prev <- d[Population == 'All', as.character(2017:2021), with = F]
prev <- data.table(cbind(year = 2017:2021, `colnames<-`(t(prev), c('x', 'n')), propCI(prev[1,], prev[2,], form = F)))

hc <- d[Population == 'Did not seek health care treatment', as.character(2017:2021), with = F]
hc <- data.table(cbind(year = 2017:2021, `colnames<-`(t(hc), c('x', 'n')), propCI(hc[1,], hc[2,], form = F)))

mx <- 0.8

#png('uam_ssti_prev_17oct2022.png', height = 7, width = 4, units = 'in', res = 300, family = 'Franklin Gothic Book')
emf('uam_ssti_prev_17oct2022.emf', height = 7, width = 4, family = 'Franklin Gothic Book')

par(mfrow = c(2, 1), mar = c(3, 3, 2, 0), xpd = NA)

plot(1, type = 'n', xlim = c(2017, 2022), ylim = c(0, mx), axes = F, xlab = NA, ylab = NA)
axis(2, 0:(mx * 10)/10, paste0(0:(mx * 10) * 10, '%'), pos = 2017, las = 2)
axis(1, 2017:2022, pos = 0, labels = F)
axis(1, 2017:2021 + 0.5, pos = 0, labels = 2017:2021, tick = F)
text(2019.5, mx, 'A: Proportion reporting symptoms\nof an injection-site infection')
with(prev, {
  rect(2017:2021 + 0.1, 0, 2018:2022 - 0.1, prop, col = viridis(5)[3])
  arrows(2017:2021 + 0.5, lower, y1 = upper, code = 3, angle = 90, length = 0.08)
})

plot(1, type = 'n', xlim = c(2017, 2022), ylim = c(0, mx), axes = F, xlab = NA, ylab = NA)
axis(2, 0:(mx * 10)/10, paste0(0:(mx * 10) * 10, '%'), pos = 2017, las = 2)
axis(1, 2017:2022, pos = 0, labels = F)
axis(1, 2017:2021 + 0.5, pos = 0, labels = 2017:2021, tick = F)
text(2019.5, mx, 'B: Of those with symptoms,\nproportion that did not\nseek medical care')
with(hc, {
  rect(2017:2021 + 0.1, 0, 2018:2022 - 0.1, prop, col = viridis(5)[3])
  arrows(2017:2021 + 0.5, lower, y1 = upper, code = 3, angle = 90, length = 0.08)
})

dev.off()

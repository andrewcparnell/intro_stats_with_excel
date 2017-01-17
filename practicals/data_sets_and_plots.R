# Data sets used for DAFM course
#setwd("/Volumes/MacintoshHD2/GDrive/IDA/DAFM/class_notes")
setwd("~/GitHub/intro_stats_with_excel/class_notes")

# Data set 1
cows = c(169.6,142,103.3,111.6,123.4,143.5,155.1,101.7,170.7,113.2,130.9,146.1,169.3,155.5)

library(xtable)
xtable(matrix(cows))

# Data set 2

# Create data with breeding value vs genomic expression

set.seed(123)
N = 20
expression = sort(round(runif(N, 0, 2),2))
gbv = round(3 + 2 * expression + rnorm(N), 2)

plot(expression, gbv)
dat = cbind(expression, gbv)[sample(1:N),]

print(dat)

xtable(head(dat))

# hyp test
se = sd(cows)/sqrt(14)
test = (mean(cows) - 120)/se

x = seq(-3, 3, length = 100)
plot(x, dnorm(x), type = 'l', xlab = '', ylab = '', las = 1, main = 'A standard normal distribution')
abline(v = test, col = 'red')

# Create a t-distribution with 13df for class 13 and shade the 95% region

x = seq(-3, 3, length = 100)
pdf(file = 't_plot.pdf', width = 6, height = 3)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(x, dt(x, df = 13), type = 'l', xlab = '', ylab = '')#, main = 't-distribution with 13 degrees of freedom')
lims = c(qt(0.025, 13), qt(0.975, 13))
x2 = seq(lims[1], lims[2], length = 100)
y2 = dt(x2, df = 13)
polygon(c(x2, rev(x2)), c(rep(0, 100), y2), col = 'lightblue', border = NA)
text(0, 0.2, '95% area')
dev.off()

pdf(file = 't_plot2.pdf', width = 6, height = 3)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(x, dt(x, df = 13), type = 'l', xlab = '', ylab = '')#, main = 't-distribution with 13 degrees of freedom')
lims = c(qt(0.025, 13), qt(0.975, 13))
x2 = seq(lims[1], lims[2], length = 100)
y2 = dt(x2, df = 13)
polygon(c(x2, rev(x2)), c(rep(0, 100), y2), col = 'lightblue', border = NA)
text(0, 0.2, '95% area')
abline(v = 2.78, col = 'red')
text(2, 0.3, 'test statistic\nvalue', col = 'red')
dev.off()

pdf(file = 'SamplingDistribution.pdf', width = 6, height = 3)
mean = 0.3
n = 190
se = 0.7/sqrt(190)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
set.seed(123)
dens = rnorm(10000, mean = mean, sd = se)
hist(dens, freq = FALSE, xlab = 'Difference in hours of sleep (females - males)', ylab = '', main = '')
x = seq(0, 1, length = 100)
lines(x, dnorm(x, mean= mean, sd = se), col = 'red')
dev.off()

pdf(file = 'normal_areas.pdf', width = 6, height = 3)
x = seq(-3, 3, length = 100)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(x, dnorm(x), type = 'l', xlab = '', ylab = '')#, main = 't-distribution with 13 degrees of freedom')
lims = c(qnorm(0.025), qnorm(0.975))
x2 = seq(lims[1], lims[2], length = 100)
y2 = dnorm(x2)
polygon(c(x2, rev(x2)), c(rep(0, 100), y2), col = 'lightblue', border = NA)
text(0, 0.2, '95% area')
text(-2.5, 0.1, '2.5% area', col = 'red')
text(2.5, 0.1, '2.5% area', col = 'red')
x3 = seq(qnorm(0.975), 3, length = 100)
y3 = dnorm(x3)
polygon(c(x3, rev(x3)), c(y3, rep(0, 100)), col = 'red', border = NA)
x4 = seq(-3, qnorm(0.025), length = 100)
y4 = dnorm(x4)
polygon(c(x4, rev(x4)), c(y4, rep(0, 100)), col = 'red', border = NA)
dev.off()

pdf(file = 't_plot3.pdf', width = 6, height = 3)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
plot(x, dt(x, df = 13), type = 'l', xlab = '', ylab = '')#, main = 't-distribution with 13 degrees of freedom')
lims = c(qt(0.025, 13), qt(0.975, 13))
x2 = seq(lims[1], lims[2], length = 100)
y2 = dt(x2, df = 13)
polygon(c(x2, rev(x2)), c(rep(0, 100), y2), col = 'lightblue', border = NA)
text(0, 0.2, '95% area')
abline(v = lims[2], lty = 'dotted')
dev.off()

pdf(file = 'anova_plot.pdf', width = 6, height = 3)
par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01,las=1)
dat = c(0.953,0.953,0.927,0.912,1.016,0.926,1.194,1.07,1.192,1.127,1.086,1.074,0.95,0.886,0.98,1.019,0.915,0.923)
run = c(rep(1,6),rep(2,6),rep(3,6))
plot(run, dat, pch = 19, col = run, xlab = 'Run', ylab = 'Clenbuterol value', xaxt = 'n', yaxt = 'n', xlim = c(0.5, 3.5))
axis(2, at = seq(0.9, 1.2, by = 0.1))
axis(1, at = 1:3)
abline(h = mean(dat))
grid()
dev.off()

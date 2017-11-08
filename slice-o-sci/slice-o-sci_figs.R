library(viridis)

setwd('~/Dropbox/Research/notes/slice-o-sci')

xy <- jitter(as.matrix(expand.grid(1:10, 1:10)), factor = 3)


nMean <- 30
xMean <- sort(runif(nMean, 2, 9)) + rnorm(nMean, sd = 0.25)
yMean <- sort(runif(nMean, 3, 7)) + rnorm(nMean, sd = 0.25)

xMale <- sort(runif(nMean, 2, 5)) + rnorm(nMean, sd = 0.15)
yMale <- sort(runif(nMean, 3, 4)) + rnorm(nMean, sd = 0.15)

xFemale <- sort(runif(nMean, 2, 10)) + rnorm(nMean, sd = 0.25)
yFemale <- sort(runif(nMean, 3, 10)) + rnorm(nMean, sd = 0.25)

xPop <- sort(runif(nMean, 2, 3.5)) + rnorm(nMean, sd = 0.15)
yPop <- sort(runif(nMean, 3, 3.25)) + rnorm(nMean, sd = 0.15)


pdf('fig_songSpace.pdf', width = 4, height = 4)
par(mar = c(2, 2, 0, 0) + 0.2, mgp = c(1, 1, 0))
plot(xy, xlab = 'Socio-relevant axis 1', ylab = 'Socio-relevant axis 2', 
     axes = FALSE, frame.plot = TRUE)
dev.off()

pdf('fig_songSpace_meanEvol.pdf', width = 4, height = 4)
par(mar = c(2, 2, 0, 0) + 0.2, mgp = c(1, 1, 0))
plot(xy, xlab = 'Socio-relevant axis 1', ylab = 'Socio-relevant axis 2', 
     axes = FALSE, frame.plot = TRUE)
segments(x0 = xMean[-nMean], y0 = yMean[-nMean], 
         x1 = xMean[-1], y1 = yMean[-1], col = viridis(nMean), lwd = 3)
dev.off()


pdf('fig_songSpace_genderEvol.pdf', width = 4, height = 4)
par(mar = c(2, 2, 0, 0) + 0.2, mgp = c(1, 1, 0))
plot(xy, xlab = 'Socio-relevant axis 1', ylab = 'Socio-relevant axis 2', 
     axes = FALSE, frame.plot = TRUE)
segments(x0 = xMean[-nMean], y0 = yMean[-nMean], 
         x1 = xMean[-1], y1 = yMean[-1], col = viridis(nMean), lwd = 3)
segments(x0 = xMale[-nMean], y0 = yMale[-nMean], 
         x1 = xMale[-1], y1 = yMale[-1], col = magma(nMean), lwd = 3, 
         lty = 4)
segments(x0 = xFemale[-nMean], y0 = yFemale[-nMean], 
         x1 = xFemale[-1], y1 = yFemale[-1], col = magma(nMean), lwd = 3, 
         lty = 1)
dev.off()

pdf('fig_songSpace_PopEvol.pdf', width = 4, height = 4)
par(mar = c(2, 2, 0, 0) + 0.2, mgp = c(1, 1, 0))
plot(xy, xlab = 'Socio-relevant axis 1', ylab = 'Socio-relevant axis 2', 
     axes = FALSE, frame.plot = TRUE)
segments(x0 = xMean[-nMean], y0 = yMean[-nMean], 
         x1 = xMean[-1], y1 = yMean[-1], col = viridis(nMean), lwd = 3)
segments(x0 = xPop[-nMean], y0 = yPop[-nMean], 
         x1 = xPop[-1], y1 = yPop[-1], col = magma(nMean), lwd = 3)
dev.off()




library(alphashape3d)

x <- matrix(rnorm(300), ncol = 3)
alpha <- 10
ashape3d.obj <- ashape3d(x, alpha = alpha)
plot(ashape3d.obj, col = rep(hsv(0.4, 0.5, 0.7), 3))
rgl.postscript('fig_femaleHull.pdf', fmt = 'pdf')


x <- matrix(rnorm(300, sd = 3), ncol = 3)
alpha <- 5
ashape3d.obj <- ashape3d(x, alpha = alpha)
plot(ashape3d.obj, col = rep(hsv(0.12, 0.7, 0.6), 3))
rgl.postscript('fig_maleHull.pdf', fmt = 'pdf')


library(meteR)

psi <- function(x, la1, la2) {
    sum(log(la2 * (la1 + la2) * exp(-la1 - la2 * x) / (1 - exp(-la1 - la2 * x))^2))
}

esf <- meteESF(S0 = 20, N0 = 200, E0 = 8000)
x <- meteDist2Rank(ipd(esf))
plot(x)

La <- as.matrix(expand.grid(la1 = seq(0.005, 0.09, length.out = 40), 
                            la2 = seq(0.001, 0.01, length.out = 40)))

mle <- sapply(1:nrow(La), function(i) {
    psi(x, La[i, 1], La[i, 2])
})

pdf('fig_psiFit.pdf', width = 4, height = 4)
par(mar = c(3, 3, 0.4, 0.4) + 0.5, mgp = c(2, 0.5, 0), cex.lab = 1.5)
image(unique(La[, 1]), unique(La[, 2]), matrix(mle, nrow = 40), col = plasma(50), 
      xlab = expression(lambda[1]), ylab = expression(lambda[2]))
contour(unique(La[, 1]), unique(La[, 2]), matrix(mle, nrow = 40), add = TRUE, 
        labels = '', labcex = 0.1)
dev.off()


## xy of plants
bci <- read.csv('../../MaxEnt_MaxHeight/ctfs_data/BCIS.csv', as.is = TRUE)

pdf('fig_orgXY.pdf', width = 4, height = 3)
par(mfrow = c(2, 2), mar = rep(0.1, 4), xaxt = 'n', yaxt = 'n')
plot(bci[bci$spp == 'SOROAF', c('x', 'y')], pch = 16, cex = 0.5)
plot(bci[bci$spp == 'TALINE', c('x', 'y')], pch = 16, cex = 0.5)
plot(bci[bci$spp == 'LACMPA', c('x', 'y')], pch = 16, cex = 0.5)
plot(bci[bci$spp == 'INGAPU', c('x', 'y')], pch = 16, cex = 0.5)
dev.off()

xy <- expand.grid(1:4, 1:3)
ones <- numeric(nrow(xy))
ones[sample(length(ones), 4)] <- 1
ones[ones == 0] <- -1
plot(xy[ones == 1, ], pch = 16, cex = 2, 
     xlim = range(xy[, 1]) + c(-0.5, 0.5), ylim = range(xy[, 2]) + c(-0.5, 0.5))
abline(v = (1:5) - 0.5, h = (1:4) - 0.5)

plot(xy, type = 'n', 
     xlim = range(xy[, 1]) + c(-0.5, 0.5), ylim = range(xy[, 2]) + c(-0.5, 0.5))
abline(v = (1:5) - 0.5, h = (1:4) - 0.5)

text(xy, labels = ones, cex = 3)

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

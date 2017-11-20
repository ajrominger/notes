library(plotrix)
library(RColorBrewer)
library(sp)
library(rgdal)
library(raster)
library(socorro)
library(kokua)
library(viridis)

setwd('~/Dropbox/Research/notes/eco-evo-wswg')

## set-up colors
rain.colors <- colorRampPalette(rev(viridis(20)))

## get precip
oldwd <- setwd('~/Dropbox/hawaiiDimensions/geodata/env_data/precip/StateRFGrids_mm2/staterf_mmann')
precip <- raster('w001001.adf')
precipP <- rasterToPolygons(cut(precip, breaks = 20), dissolve = TRUE)
precipP <- spTransform(precipP, CRS(proj4string(islands)))
setwd(oldwd)


pdf('fig_precip.pdf', width = 4, height = 4)
par(mar=rep(0, 4), bg = 'black')
plot(islands)
plot(precipP, col = rain.colors(20)[precipP$layer], border = rain.colors(20)[precipP$layer], 
     add = TRUE)
plot(islands, border = 'white', bg = 'transparent', add = TRUE)
dev.off()

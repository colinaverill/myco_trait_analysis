rm(list=ls())
source('paths.r')

#set output path.----
output.path <- GBIF_vs_WC2.path
#load data, fit models.----
d <- readRDS(inter_specific_analysis_data.path)

#gbif vs. worldclim relatipnship breaks down at very high levels of precip > 3000mm.
#all the actual values of gbif we used were less than 3000.
#subset to rows where gbif was used, grab the largest gbif_precip value among them.
gbif_max <- max(d[is.na(d$map),]$gbif_precip, na.rm = T)

#Fit two models.
mat.mod <- lm(mat ~ gbif_temp, data = d)
map.mod <- lm(map ~ gbif_precip, data = d[d$gbif_precip < 3000,])

#output spec.----
png(filename=output.path,width=8,height=4,units='in',res=300)

#Plot it up.----
par(mfrow = c(1,2),
    mar = c(4,4,1,1))
#temperature plot
plot(gbif_temp ~ mat, data = d, xlab = 'WorldClim2 MAT', ylab = 'GBIF MAT', pch = 16, cex = 0.8)
abline(mat.mod, lwd = 2)
abline(0,1, lwd = 2, lty = 2, col = 'purple')
rsq <- round(summary(mat.mod)$r.squared, 2)
mtext(bquote(R^2 == .(rsq)), side = 3, adj = 0.05, line = -1.5)
#precipitation plot
plot(map ~ gbif_precip, data = d[d$gbif_precip < 3000,], xlab = 'WorldClim2 MAP', ylab = 'GBIF MAP', pch = 16, cex = 0.8)
abline(map.mod, lwd = 2)
abline(0,1, lwd = 2, lty = 2, col = 'purple')
rsq <- round(summary(map.mod)$r.squared, 2)
mtext(bquote(R^2 == .(rsq)), side = 3, adj = 0.05, line = -1.5)

#end plot.----
dev.off()

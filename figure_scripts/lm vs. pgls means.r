#Plotting lm vs. PGLS results of tissue chemistry traits.
#clear envrionment, load packages.
rm(list=ls())
library(data.table)
source('paths.r')
source('functions/p_report.r')

#set output path.----
output.path <- lm_pgls_effects_figure.path
#load summary data for figure.----
d <- data.table(readRDS(lm_pgls_means_myc.pgf_merged.clim_summary.path))

#get upper and lower AM and ECM values.
d$am.upr <- d$AM + d$error
d$am.lwr <- d$AM - d$error
d$em.upr <- d$ECM + d$error
d$em.lwr <- d$ECM - d$error


#setup to save.----
png(filename=output.path,width=9,height=10,units='in',res=300)

#global plot settings.-----
#setup plot panels and margins.
par(mfrow=c(3,3),
    mar = c(1,2,1,2),
    oma = c(6,6,1,1))
#get x positions
x <- c(0.9,1.05,1.4,1.55)
limx <- c(0.8,1.7)
a.cex <- 1.2
#pick colors.
cols <- c('#00acd9','#cfe83c')

#Leaf Lifespan.----
traits <- c('log.LL.lm','log.LL.pg')
units <- expression(paste('months'))
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext('Leaf Lifespan', side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)
mtext(units, side =2, cex = 1.3, line = 2.5)

#Root Lifespan.----
traits <- c('root.L.lm','root.L.pg')
name <- 'Root Lifespan'
units <- expression(paste('log(months)'))
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)
#mtext(units, side =2, cex = 1.3, line = 2.5)

#Drop empty plot to place legend in.----
plot(means~x, data = d,xaxt='n',yaxt='n',bty='n',pch='',ylab='',xlab='',ylim = c(0,10), xlim = c(0,10))
legend(0, 10,legend = c('arbuscular \nmycorrhizal','ectomycorrhizal'), pch = 16, col = c(cols[1],cols[2]), bty = 'n', x.intersp = 0.7, y.intersp = 1.5, cex = 2)


#N green.----
traits <- c('Ngreen.lm','Ngreen.pg')
units <- expression(paste('mg N (g tissue)'^'-1'))
name <- 'Nitrogen  Green'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)
mtext(units, side =2, cex = 1.3, line = 2.5)


#N senes.----
traits <- c('Nsenes.lm','Nsenes.pg')
units <- expression(paste('mg N (g tissue)'^'-1'))
name <- 'Nitrogen  Senescent'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)

#N roots.----
traits <- c('Nroots.lm','Nroots.pg')
units <- expression(paste('mg N (g tissue)'^'-1'))
name <- 'Nitrogen  Roots'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)

#P green.----
traits <- c('Pgreen.lm','Pgreen.pg')
units <- expression(paste('mg P (g tissue)'^'-1'))
name <- 'Phosphorus  Green'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)
mtext(units, side =2, cex = 1.3, line = 2.5)

#P senes.----
traits <- c('Psenes.lm','Psenes.pg')
units <- expression(paste('mg P (g tissue)'^'-1'))
name <- 'Phosphorus  Senescent'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)

#P roots.----
traits <- c('Proots.lm','Proots.pg')
units <- expression(paste('mg P (g tissue)'^'-1'))
name <- 'Phosphorus  Roots'
means <- 10^c(    d[trait == traits[1],AM],    d[trait == traits[1],ECM],    d[trait == traits[2],AM],   d[trait == traits[2],ECM])
upr   <- 10^c(d[trait == traits[1],am.upr], d[trait == traits[1],em.upr],d[trait == traits[2],am.upr],d[trait == traits[2],em.upr])
lwr   <- 10^c(d[trait == traits[1],am.lwr], d[trait == traits[1],em.lwr],d[trait == traits[2],am.lwr],d[trait == traits[2],em.lwr])
p     <- c(d[trait == traits[1],p_val] , d[trait == traits[2], p_val])
am.n  <- d[trait == traits[1], N_AM]
em.n  <- d[trait == traits[1],N_ECM]
limy <- c(0,max(means)*1.5)
plot(means ~ x, pch = 16, xaxt='n', cex = 0, ylim = limy, xlim = limx, ylab = NA, col = cols, cex.axis = a.cex)
arrows(x, lwr, x, upr, length=0.05, angle=90, code=3)
points(means ~ x, pch = 16, col = cols, cex = 2)
abline(v=((limx[2] - limx[1])/2) + limx[1], lwd = 2, lty = 2, col = 'light gray')
mtext(name, side = 3, adj = 0.05, line = -2)
mtext(bquote('AM n' == .(am.n)), line = -4, adj = 0.05)
mtext(bquote('EM n' == .(em.n)), line = -6, adj = 0.05)
mtext(p_report(p[1]), side = 1, line = -1.5, adj = 0.15)
mtext(p_report(p[2]), side = 1, line = -1.5, adj = 0.8)


#outer labels.----
mtext('without', side = 1, outer = T, cex = 1.2, line = .5, adj = 0.07)
mtext('correction', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.05)
mtext('phylo-', side = 1, outer = T, cex = 1.2, line =.5, adj = 0.21)
mtext('corrected', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.20)

mtext('without', side = 1, outer = T, cex = 1.2, line = .5, adj = 0.43)
mtext('correction', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.43)
mtext('phylo-', side = 1, outer = T, cex = 1.2, line =.5, adj = 0.57)
mtext('corrected', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.57)

mtext('without', side = 1, outer = T, cex = 1.2, line = .5, adj = 0.79)
mtext('correction', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.80)
mtext('phylo-', side = 1, outer = T, cex = 1.2, line =.5, adj = 0.94)
mtext('corrected', side = 1, outer = T, cex = 1.2, line = 2, adj = 0.95)

#end plot.----
dev.off()
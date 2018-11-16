#comparing mycorrhizal effect sizes among models including no climate, worldclim, or gbif.
rm(list=ls())
source('paths.r')
d1 <- readRDS(lm_pgls_means_myc.pgf_summary.path)
d2 <- readRDS(lm_pgls_means_myc.pgf.clim_summary.path)
d3 <- readRDS(lm_pgls_means_myc.pgf.gbif_summary.path)
test <- data.frame(d1$MYCO_ASSO, d2$MYCO_ASSO)
colnames(test) <- c('no_clim','clim')

#plot panels.
par(mfrow = c(1,3))

#No climate vs. worldclim.
test <- data.frame(d1$MYCO_ASSO, d2$MYCO_ASSO)
colnames(test) <- c('no_clim','clim')
plot(clim ~ no_clim, data = test[test$no_clim < 150,],
     ylab = 'with worldclim', xlab = 'no climate', cex.lab = 1.5)
abline(lm(clim ~ no_clim, data = test[test$no_clim < 150,]), lwd = 2)
rsq <- round(summary(lm(clim ~ no_clim, data = test[test$no_clim < 150,]))$r.squared, 2)
mtext(paste0('R2=',rsq), side =3, adj = 0.05, line = -1.5)

#No climate vs. GBIF.
test <- data.frame(d1$MYCO_ASSO, d3$MYCO_ASSO)
colnames(test) <- c('no_clim','clim')
plot(clim ~ no_clim, data = test[test$no_clim < 150,],
     ylab = 'with GBIF climate envelope', xlab = 'no climate', cex.lab = 1.5)
abline(lm(clim ~ no_clim, data = test[test$no_clim < 150,]), lwd = 2)
rsq <- round(summary(lm(clim ~ no_clim, data = test[test$no_clim < 150,]))$r.squared, 2)
mtext(paste0('R2=',rsq), side =3, adj = 0.05, line = -1.5)

#Worldclim vs. GBIF climate envelope.
test <- data.frame(d2$MYCO_ASSO, d3$MYCO_ASSO)
colnames(test) <- c('wc','gb')
plot(wc ~ gb, data = test[test$gb < 200,],
     ylab = 'worldclim', xlab = 'GBIF climate', cex.lab = 1.5)
abline(lm(wc ~ gb, data = test[test$gb < 200,]), lwd = 2)
rsq <- round(summary(lm(wc ~ gb, data = test[test$gb < 200,]))$r.squared, 2)
mtext(paste0('R2=',rsq), side =3, adj = 0.05, line = -1.5)



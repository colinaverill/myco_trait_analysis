#comparing mycorrhizal effect sizes among models including no climate, worldclim, or gbif.
rm(list=ls())
source('paths.r')
d1 <- readRDS(lm_pgls_means_myc.pgf_summary.path)
d2 <- readRDS(lm_pgls_means_myc.pgf.clim_summary.path)
d3 <- readRDS(lm_pgls_means_myc.pgf.gbif_summary.path)
d4 <- readRDS(lm_pgls_means_myc.pgf_merged.clim_summary.path)

#noramlize effect sizes.
d1.effect <- (d1$MYCO_ASSO + d1$Intercept) / d1$Intercept
d2.effect <- (d2$MYCO_ASSO + d2$Intercept) / d2$Intercept
d3.effect <- (d3$MYCO_ASSO + d3$Intercept) / d3$Intercept
d4.effect <- (d4$MYCO_ASSO + d4$Intercept) / d4$Intercept

#compare climate coefficiesnts.
pairs(data.frame(d2.effect,d3.effect,d4.effect))


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
plot(wc ~ gb, data = test[test$gb < 70,],
     ylab = 'worldclim', xlab = 'GBIF climate', cex.lab = 1.5)
abline(lm(wc ~ gb, data = test[test$gb < 70,]), lwd = 2)
rsq <- round(summary(lm(wc ~ gb, data = test[test$gb < 70,]))$r.squared, 2)
mtext(paste0('R2=',rsq), side =3, adj = 0.05, line = -1.5)


#merged climate vs. without climate effects.
test <- data.frame(d1$MYCO_ASSO, d4$MYCO_ASSO)
colnames(test) <- c('no.clim','clim')
par(mfrow = c(1,1))
plot(clim ~ no.clim, data = test[test$no.clim < 70,])
mod <- lm(clim ~ no.clim, data = test[test$no.clim < 70,])
abline(mod, lwd = 2)
rsq <- round(summary(mod)$r.squared, 2)
mtext(paste0('R2=',rsq), side =3, adj = 0.05, line = -1.5)

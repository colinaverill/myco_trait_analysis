#plot parameter estimates of models with with wc2 or gbif climate values.
#clear environment, load packages and paths.
rm(list=ls())
source('paths.r')

#load fits, isolate pgls fits, get summaries.----
wc <- readRDS(lm_pgls_means_myc.pgf_wc2_models.path)
gb <- readRDS(lm_pgls_means_myc.pgf_gbif_models.path)
mc <- readRDS(lm_pgls_means_myc.pgf_merged.clim_models.path)
wc <- wc[9:16]
gb <- gb[9:16]
mc <- mc[9:16]
wc <- lapply(wc,caper::summary.pgls)
gb <- lapply(gb,caper::summary.pgls)
mc <- lapply(mc,caper::summary.pgls)

#Get vectors of mat and map parameters for each fit.
gb.mat <- list()
gb.map <- list()
wc.mat <- list()
wc.map <- list()
mc.mat <- list()
mc.map <- list()
for(i in 1:length(gb)){
  gb.mat[[i]] <- gb[[i]]$coefficients[c('mat.c'),1]
  gb.map[[i]] <- gb[[i]]$coefficients[c('map.c'),1]
  wc.mat[[i]] <- wc[[i]]$coefficients[c('mat.c'),1]
  wc.map[[i]] <- wc[[i]]$coefficients[c('map.c'),1]
  mc.mat[[i]] <- mc[[i]]$coefficients[c('mat.c'),1]
  mc.map[[i]] <- mc[[i]]$coefficients[c('map.c'),1]
}
gb.mat <- unlist(gb.mat)
gb.map <- unlist(gb.map)
wc.mat <- unlist(wc.mat)
wc.map <- unlist(wc.map)
mc.mat <- unlist(mc.mat)
mc.map <- unlist(mc.map)

par(mfrow=c(1,2))
plot(gb.mat ~ mc.mat);abline(0,1)
plot(wc.mat ~ mc.mat);abline(0,1)

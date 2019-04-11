#Plot AM-EM trait means across latitudinal zones.
rm(list=ls())
source('paths.r')
source('functions/p_report.r')
library(data.table)
library(caper)

#load data.----
d <- readRDS(pgls.glmm_myc.biome3_interaction_no.selection.path)

#subset and order the list.----
list_order <- list('log.LL','root_lifespan','Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots')
names(list_order) <- c('log.LL','root_lifespan','Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots')
d <- d[names(list_order)]

#Go get values, AM-EM differences.
em.check <- list()
am.check <- list()
for(i in 1:length(d)){
  am <- d[[i]]$mean[c(1,3,5)]
  em <- d[[i]]$mean[c(2,4,6)]
  am <- 10^am
  em <- 10^em
  em.diff <- round((((em - am) / am)*100), 2)
  am.diff <- round((((am - em) / em)*100), 2)
  em.check[[i]] <- em.diff
  am.check[[i]] <- am.diff
}
em.check <- do.call(rbind, em.check)
am.check <- do.call(rbind, am.check)
rownames(em.check) <- names(d)
colnames(em.check) <- c('boreal','temperate','tropic')

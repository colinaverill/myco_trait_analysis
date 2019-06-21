#Using MCMCglmm to fit PGLS models to NP ratios of tissues.
#zero model selection performed, just straight up sending model structures based on apriori hypotheses.
#you need to decide whether or not to log10 transform.
rm(list=ls())
source('paths.r')
source('functions/pgls_glmm_no_selection.r')
source('functions/tic_toc.r')
library(data.table)
library(phytools)
library(MCMCglmm)
library(caper)


#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path) #'colin_2018-12--2.tre'

#Some data prep.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
d <- d[d$Species %in% phy$tip.label,]
#ECM origins at family level.
ecm <- list()
ecm[[1]] <- c('Pinaceae')
ecm[[2]] <- c('Fabaceae','Leguminosae')
ecm[[3]] <- c('Fagaceae','Betulaceae','Nothofagaceae')
ecm[[4]] <- c('Salicaceae')
ecm[[5]] <- c('Dipterocarpaceae')
ecm[[6]] <- c('Myrtaceae')
ecm[[7]] <- c('Phyllanthaceae')

#Based on Stevens P. 2013. Angiosperm phylogeny website. Version 12, July 2012
#And another reference for Pinaceae age.
#These seem too close. No obvious patterns in N content w/o phylo correct.
ages <- c('225','93.2','96.6','87.61','50.3','86.38','93.46')

#Get a new ECM predictor.
d$myco_2 <- as.character(d$MYCO_ASSO)
for(i in 1:length(ecm)){
  lab <- paste0('ECM_',i)
  d$myco_2 <- ifelse(d$tpl.Family %in% ecm[[i]], lab, d$myco_2)
}

#quick and dirty.

#within gymnosperms - Ngreen higher among ECM species, but Nsenes and N roots lower!!!
mod <- lm((Nroots) ~ myco_2 + nfix  + mat.c + map.c, data = d)
   #       data = d[d$pgf == 'gymno',])
summary(mod)


form <- as.formula(paste0('Nsenes ~ myco_2 + nfix + mat.c + map.c'))
fit <- pgls_glmm(form, d, phy)
form <- as.formula(paste0('Nroots ~ MYCO_ASSO + nfix + mat.c + map.c'))
fit <- pgls_glmm(form, d[d$pgf == 'gymno',], phy)


par(mfrow = c(1,2))
hist(d[d$pgf == 'gymno' & d$MYCO_ASSO=='ECM',]$Ngreen)
hist(d[d$pgf == 'gymno' & d$MYCO_ASSO== 'AM',]$Ngreen)


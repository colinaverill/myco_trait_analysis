#Fitting all trait correlations, excluding RL is 21 models.
#First set - corrleation x mycorrhizal type.
#Second set - correlation x mycorrhizal type + covariates.
rm(list=ls())
source('paths.r')
source('functions/pgls_glmm_no_selection.r')
source('functions/tic_toc.r')
library(data.table)
library(phytools)
library(MCMCglmm)
library(caper)

#set output path.----
output.path <- pgls.glmm_myc_trait_coordination_analysis.path

#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path) #'colin_2018-12--2.tre'
d$biome_trop <- ifelse(d$biome3 == 'b_tropical',1, 0)
d$biome_bore <- ifelse(d$biome3 == 'c_boreal',1,0)
d$Nresor <- d$Ngreen - d$Nsenes
d$Presor <- d$Pgreen - d$Psenes

#Some data prep.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
d <- d[d$Species %in% phy$tip.label,]
d$MYCO_ASSO <- droplevels(d$MYCO_ASSO)

#Specify model structure----
#specify traits.
traits <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL')
traits <- c('Nroots','Psenes','Proots') #for testing.

#Specify two sets of predictors.
predictors.1 <- '*MYCO_ASSO'
predictors.2 <- '*MYCO_ASSO + nfix + pgf + mat.c + map.c'


#Define all possible trait correlations.
models <- c()
match <- traits
for(i in 1:length(traits)){
  trait <- traits[i]
  match <- match[!(match %in% trait)]
  if(length(match) == 0){next}
  to_return <- list()
  for(k in 1:length(match)){
    
    to_return[[k]] <- paste0('log10(',trait,') ~ log10(',match[k],')')
    if(match[k] == 'log.LL'){
      to_return[[k]] <- paste0('log10(',trait,') ~ ',match[k])
    }
  }
  to_return <- unlist(to_return)
  models <- c(models,to_return)
}

#Define the two sets of models.
models.1 <- paste0(models, predictors.1)
models.2 <- paste0(models, predictors.2)

#Fit models without covariates.----
model.1.output <- list()
tic()
cat('Fitting models without covariates...\n')
for(i in 1:length(models.1)){
  form <- as.formula(models.1[i])
  fit <- pgls_glmm(form, d, phy)
  model.1.output[[i]] <- fit
}
cat('Models without covariates fit.\n')
toc()

#Fit models with covariates.----
model.2.output <- list()
tic()
cat('Fitting models with covariates...\n')
for(i in 1:length(models.2)){
  form <- as.formula(models.2[i])
  fit <- pgls_glmm(form, d, phy)
  model.2.output[[i]] <- fit
}
cat('Models with covariates fit.\n')
toc()

#wrap output and save.----
output <- list(model.1.output, model.2.output)
names(output) <- c('no.cov','yes.cov')
saveRDS(output, output.path)


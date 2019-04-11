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

#set output path.----
output.path <- pgls.glmm_NP_analysis.path

#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path) #'colin_2018-12--2.tre'
d$biome_trop <- ifelse(d$biome3 == 'b_tropical',1, 0)
d$biome_bore <- ifelse(d$biome3 == 'c_boreal',1,0)
d$greenNP <- (d$Ngreen / d$Pgreen)
d$senesNP <- d$Nsenes / d$Psenes
d$rootsNP <- d$Nroots / d$Proots

#Some data prep.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
d <- d[d$Species %in% phy$tip.label,]
d$MYCO_ASSO <- droplevels(d$MYCO_ASSO)


#Specify model structure (predictors) and traits----
predictors <- 'MYCO_ASSO + MYCO_ASSO:biome_bore + MYCO_ASSO:biome_trop + biome_trop + biome_bore + nfix + pgf + mat.c + map.c'
traits <- c('greenNP','senesNP','rootsNP')

#Fit models.----
#log10 models.
output.log10 <- list()
for(i in 1:length(traits)){
  form <- as.formula(paste0('log10(',traits[i],') ~ ',predictors))
  fit <- pgls_glmm(form, d, phy)
   output.log10[[i]] <- fit
}
names(output.log10) <- traits

#untransformed.
output.reg <- list()
for(i in 1:length(traits)){
  form <- as.formula(paste0('(',traits[i],') ~ ',predictors))
  fit <- pgls_glmm(form, d, phy)
  output.reg[[i]] <- fit
}
names(output.reg) <- traits

#save output.----
output <- list(output.log10,output.reg)
names(output) <- c('log10','reg')
saveRDS(output, output.path)

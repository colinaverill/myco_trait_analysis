#AICC comparison across traits.
rm(list=ls())
library(data.table)
library(doParallel)
library(caper)
source('paths.r')
source('functions/pic_pro.r')
source('functions/tic_toc.r')
source('functions/pgls_step_aicc.r')

#set output path.----
output.path <- aicc_model_comparison_plgs.path

#register parallel environment.----
n.cores <- 12

#load data and phylogeny.----
d <- data.table(readRDS(inter_specific_analysis_data.path))
phy <- read.tree(phylogeny_raw.path)

#Some data prep that should really be done somewhere else.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
d$myco_nfix <- ifelse(d$MYCO_ASSO == 'ECM',1,0) * d$nfix
setnames(d,'root_lifespan','root.L')

#Loop of traits of interest.----
aic_compare <- list()
traits <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root.L')
tic()
for(i in 1:length(traits)){
  preds <- c('pgf','nfix','mat.c','map.c','biome2')
  if(traits[i] == 'root.L'){
    preds <- c('pgf','mat.c','map.c','biome2') #no nfixers among root chem observations.
  }
  aic_compare[[i]] <- pgls_step_aicc(y = traits[i], x = preds, data = d, phylogeny = phy, n.cores = n.cores, log = T)
  cat(traits[i],'comparison complete.\n')
}
toc()

#name the list, save output.
names(aic_compare) <- traits

#save output.----
saveRDS(aic_compare,output.path)

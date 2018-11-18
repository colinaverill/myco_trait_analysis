#Test for phylogenetic signal in any two traits
#clear R environment, source functions and packages.
rm(list=ls())
library(data.table)
source('paths.r')
source('functions/pic_pro.r')
source('functions/tic_toc.r')
source('functions/wrap_results_lm.r')
source('functions/wrap_results_pg.r')

#set output path.----
output.path <- biome_aicc_pgls_analysis.path

#load trait data and phylogeny.----
d <- data.table(readRDS(inter_specific_analysis_data.path))
phy <- read.tree(phylogeny_raw.path)

#Some data prep that should really be done somewhere else.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
d$Species <- d$tpl.Species
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
setnames(d,'root_lifespan','root.L')

#set traits to model.----
to_model <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root.L')
aic_out <- list()
mod_out <- list()

#loop over traits, fitting models with or without biome, save models and aicc scores.
for(i in 1:length(to_model)){
  tic()
  preds1 <- c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome')
  preds2 <- c('MYCO_ASSO','nfix','pgf','mat.c','map.c')
  check <- c(to_model[i], preds1, 'Species')
  dat <- as.data.frame(d)
  dat <- dat[,colnames(dat) %in% check]
  dat <- dat[complete.cases(dat),]
  #no nfixers among root lifespan observations.
  if(to_model[i] == 'root.L'){ 
    preds1 <- c('MYCO_ASSO','pgf','mat.c','map.c','biome')
    preds2 <- c('MYCO_ASSO','pgf','mat.c','map.c')
  }
  mod1 <- pic_pro(y = to_model[i], x = preds1, trait.data = dat, phylogeny = phy)
  mod2 <- pic_pro(y = to_model[i], x = preds2, trait.data = dat, phylogeny = phy)
  aicc_return <- c(mod1$aicc, mod2$aicc)
   mod_return <- list(mod1, mod2)
  to_name <- c(to_model[i], paste0(to_model[i],'_no.biome'))
  names(aicc_return) <- to_name
  names(mod_return) <- to_name
  aic_out[[i]] <- aicc_return
  mod_out[[i]] <- mod_return
  cat(to_model[i],'fitted.',i,'of',length(to_model),'traits analyzed.\n')
  toc()
}
names(aic_out) <- to_model
names(mod_out) <- to_model

aicc_diff <- list()
for(i in 1:length(aic_out)){
  aicc_diff[[i]] <- aic_out[[i]][1] - aic_out[[i]][2]
}
aicc_diff <- unlist(aicc_diff)
names(aicc_diff) <- to_model

#save output.
output <- list(aic_out, mod_out, aicc_diff)
names(output) <- c('aicc_scores','models','aicc_diff')
saveRDS(output, output.path)

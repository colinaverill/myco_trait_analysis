#converting multi-level factors (latitudinal zone, biome) to 0-1 variables.
rm(list=ls())
library(data.table)
library(doParallel)
library(caper)
source('paths.r')
source('functions/pic_pro.r')
source('functions/tic_toc.r')
source('functions/pgls_step_aicc.r')

#set output path.----
output.path <- aicc_lat_interactions_biome_pgls.path

#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path)

#Dial data and phylogeny.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
setnames(d,'root_lifespan','root.L')


#get factors as 0-1 variables for AIC selection.----
#set temperate latitudes and temperate forests as base groups.
d.fac <- d[,c('biome2','biome3')]
d.fac$biome2 <- ifelse(d$biome2 == 'Temperate_forest','a_Temperate_forest',d.fac$biome2)
#biome 3 already preceded by a_.
#make sure they actually factors.
d.fac[] <- lapply(d.fac, as.factor)
#covert to 0-1 matrix by factor level (save for base group).
options(na.action='na.pass')
fac.num <- model.matrix( ~ biome2 + biome3, data=d.fac)
fac.num <- fac.num[,-1] #drop intercept column.

#make dataset for analysis.----
d <- data.table::data.table(d)
d <- d[,.(Species,MYCO_ASSO,pgf,nfix,mat.c,map.c,Ngreen,Pgreen,Nsenes,Psenes,Nroots,Proots,log.LL,root.L)]
d$MYCO_ASSO <- ifelse(d$MYCO_ASSO == 'ECM',1,0)
d$pgf <- ifelse(d$pgf == 'gymno',1,0)
d <- cbind(d,fac.num)
d$myco_tropical <- d$MYCO_ASSO * d$biome3b_tropical
d$myco_boreal   <- d$MYCO_ASSO * d$biome3c_boreal

#get predictor names.----
not_preds <- c('Species','Ngreen','Pgreen','Nsenes','Psenes','Nroots','Proots','log.LL','root.L')
y <- c('Ngreen','Pgreen','Nsenes','Psenes','Nroots','Proots','log.LL','root.L')
y <- c('Proots','log.LL','root.L') #testing a subset where this hung recently.
preds <- colnames(d)
preds <- preds[!(preds %in% not_preds)]

#loop function over traits.----
options(na.action='na.omit') #important!
output <- list()
for(i in 1:length(y)){
  tic()
  if(y[i] == 'log.LL'){
    output[[i]] <- try(pgls_step_aicc(y = y[i], x = preds, data = d, phylogeny = phy, n.cores = 12))
  }
  if(y[i] != 'log.LL'){
    output[[i]] <- try(pgls_step_aicc(y = y[i], x = preds, data = d, phylogeny = phy, n.cores = 12, log = T))
  }
  cat(y[i],'aicc comparison complete.\n');toc()
}

#Save output.----
saveRDS(output, output.path)

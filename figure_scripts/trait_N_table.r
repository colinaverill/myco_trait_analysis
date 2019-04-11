#counting number of observations by trait.
#Using MCMCglmm to fit PGLS models.
#zero model selection performed, just straight up sending model structures based on apriori hypotheses.
rm(list=ls())
source('paths.r')
source('functions/pgls_glmm_no_selection.r')
source('functions/tic_toc.r')
library(data.table)
library(phytools)
library(caper)

#set output path.----
output.path <- trait_N_table.path

#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path) #'colin_2018-12--2.tre'
d$biome_trop <- ifelse(d$biome3 == 'b_tropical',1, 0)
d$biome_bore <- ifelse(d$biome3 ==   'c_boreal',1, 0)

#Some data prep.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
d <- d[d$Species %in% phy$tip.label,]
d$MYCO_ASSO <- droplevels(d$MYCO_ASSO)


#specify traits, count observations.----
traits <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root_lifespan')
preds <- c('tpl.Species','MYCO_ASSO','nfix','pgf','mat.c','map.c','biome_bore','biome_trop')

sum <- list()
spp <- list()
for(i in 1:length(traits)){
  dat <- d[,colnames(d) %in% c(traits[i],preds)]
  dat <- dat[complete.cases(dat),]
  #we did not analyze tropical or boreal root lifespan observations.
  if(traits[i] == 'root_lifespan'){
    dat <- dat[dat$biome_bore == 0,]
    dat <- dat[dat$biome_trop == 0,]
  }
  
  N <- nrow(dat)
  AM <- nrow(dat[dat$MYCO_ASSO ==  'AM',])
  EM <- nrow(dat[dat$MYCO_ASSO == 'ECM',])
  nfix <- nrow(dat[dat$nfix == 1,])
  angio <- nrow(dat[dat$pgf == 'angio',])
  gymno <- nrow(dat[dat$pgf == 'gymno',])
  bore <- nrow(dat[dat$biome_bore == 1,])
  trop <- nrow(dat[dat$biome_trop == 1,])
  temp <- N - (bore + trop)
  return <- c(N,AM,EM,nfix,angio,gymno,bore,temp,trop)
  sum[[i]] <- return
  spp[[i]] <- dat$tpl.Species
}
sum <- do.call(rbind, sum)
spp <- unlist(spp)
traits <- c('green foliar N','senescent foliar N','root N',
            'green foliar P','senescent foliar P','root P',
            'leaf lifespan','root lifespan')
sum <- data.frame(cbind(traits, sum))
colnames(sum) <- c('Trait','N','AM','EM','N-fixer','angiosperm','gymnosperm','boreal','temperate','tropical')
for(i in 2:ncol(sum)){
  sum[,i] <- as.numeric(as.character(sum[,i]))
}

#save output as .csv----
write.csv(sum, output.path)


#How many total unique species?
n.spp <- length(unique(spp))
#how many trait observations?
#myc type, nfix, angio/gymno for every species, as well as the traits.
dat <- d[,c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root_lifespan')]
n.trait <- length(unique(spp)) * 3 + sum(!is.na(dat))
cat('You observed',n.trait,'traits across',n.spp,'unique species.\n')

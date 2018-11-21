#Predict traits soley based on phylogeny.
rm(list=ls())
library(picante)
library(data.table)
source('paths.r')
source('functions/phylo_predicted.r')

#set output path
out.path <- phylo_estimated_traits.path

#load trait data and phylogeny
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path)
phy <- multi2di(phy) #deal with polytomies.
setnames(d,'root_lifespan','root.L')


Ngreen <- phylo_predicted(species = d$Species, trait = d$Ngreen, phy = phy, name = 'Ngreen')
Nsenes <- phylo_predicted(species = d$Species, trait = d$Nsenes, phy = phy, name = 'Nsenes')
Nroots <- phylo_predicted(species = d$Species, trait = d$Nroots, phy = phy, name = 'Nroots')
Pgreen <- phylo_predicted(species = d$Species, trait = d$Pgreen, phy = phy, name = 'Pgreen')
Psenes <- phylo_predicted(species = d$Species, trait = d$Psenes, phy = phy, name = 'Psenes')
Proots <- phylo_predicted(species = d$Species, trait = d$Proots, phy = phy, name = 'Proots')
log.LL <- phylo_predicted(species = d$Species, trait = d$log.LL, phy = phy, name = 'log.LL')
root.l <- phylo_predicted(species = d$Species, trait = d$root.L, phy = phy, name = 'root.L')

#merge with trait data
to_merge <- list(Ngreen, Nsenes, Nroots, Pgreen, Psenes, Proots, log.LL, root.l)
out <- d
for(i in 1:length(to_merge)){
  out <- merge(out,to_merge[[i]], all.x=T)
}

#save trait data with phylogenetic predictions for each observed trait.
saveRDS(out, out.path)

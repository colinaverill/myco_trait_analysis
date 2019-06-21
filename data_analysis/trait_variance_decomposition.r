#calculating within vs. between species variance, following Anderegg et al. 2018 Ecology Letters.
rm(list=ls())
source('paths.r')
library(lme4)
library(caper)
library(MuMIn)

#set output path.----
output.path <- variance_decomp_output.path

#load data.----
d <- readRDS(intra_specific_analysis_data.path)
#Filter based on interspecific observations actually used in the analysis.
inter <- readRDS(inter_specific_analysis_data.path)
  phy <- read.tree(phylogeny_raw.path) #'colin_2018-12--2.tre'

#Some data manipulation so I can match intra-specific observations to speciesa included in interspecific analysis.
inter$biome_trop <- ifelse(inter$biome3 == 'b_tropical',1,0)
inter$biome_bore <- ifelse(inter$biome3 == 'c_boreal'  ,1,0)
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
inter <- inter[inter$Species %in% phy$tip.label,]
inter$MYCO_ASSO <- droplevels(inter$MYCO_ASSO)
inter <- inter[,c('tpl.Species','biome_trop','biome_bore','MYCO_ASSO','nfix','pgf','mat.c','map.c')]
inter <- inter[complete.cases(inter),]
  
#Filter intra-specific observations.
d <- d[d$tpl.Species %in% inter$tpl.Species,]

#subset to species that have at least 3 observations.
drop <- table(d$tpl.Species)
drop <- drop[drop >= 3]
d <- d[d$tpl.Species %in% names(drop),]

#fit lme models.----
Ngreen <- lmer(log10(Ngreen)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
Nsenes <- lmer(log10(Nsenes)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
Nroots <- lmer(log10(Nroots)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
Pgreen <- lmer(log10(Pgreen)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
Psenes <- lmer(log10(Psenes)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
Proots <- lmer(log10(Proots)        ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
log.LL <- lmer((log.LL)             ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)
root.L <- lmer(log10(root_lifespan) ~ 1 + (1|tpl.Species) + (1|tpl.Genus) + (1|tpl.Family), data = d)

#get variances.----
Ngreen_var <- data.frame(VarCorr(Ngreen))[,4]
Nsenes_var <- data.frame(VarCorr(Nsenes))[,4]
Nroots_var <- data.frame(VarCorr(Nroots))[,4]
Pgreen_var <- data.frame(VarCorr(Pgreen))[,4]
Psenes_var <- data.frame(VarCorr(Psenes))[,4]
Proots_var <- data.frame(VarCorr(Proots))[,4]
log.LL_var <- data.frame(VarCorr(log.LL))[,4]
root.L_var <- data.frame(VarCorr(root.L))[,4]
all <- data.frame(Ngreen_var,Nsenes_var,Nroots_var,Pgreen_var,Psenes_var,Proots_var,log.LL_var,root.L_var)
rownames(all) <- c('inter_species','inter_genus','inter_family','intra_species')

#Normalize variances to proportions, set order.----
for(i in 1:ncol(all)){
  all[,i] <- all[,i] / sum(all[,i])
}
my_order <- c('intra_species','inter_species','inter_genus','inter_family')
all <- all[match(my_order, rownames(all)),]

#Save output.----
saveRDS(all, output.path)

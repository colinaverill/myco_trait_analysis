#calculating within vs. between species variance, following Anderegg et al. 2018 Ecology Letters.
rm(list=ls())
source('paths.r')
library(lme4)

#set output path.----
output.path <- variance_decomp_output.path

#load data.----
d <- readRDS(intra_specific_analysis_data.path)
#d <- readRDS(inter_specific_analysis_data.path)
d <- d[d$woodiness == 'W',]
d$tpl.Genus <- gsub( " .*$", "", d$tpl.Species)

#ADD FAMILY TO MODELS. will reduce intra-specific (probably)
#fit lme models.----
Ngreen <- lmer((Ngreen) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
Nsenes <- lmer((Nsenes) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
Nroots <- lmer((Nroots) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
Pgreen <- lmer((Pgreen) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
Psenes <- lmer((Psenes) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
Proots <- lmer((Proots) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
log.LL <- lmer((log.LL) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)
root.L <- lmer((root_lifespan) ~ 1 + (1|tpl.Species) + (1|tpl.Genus), data = d)

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

#Normalize variances to proportions, set order.----
for(i in 1:ncol(all)){
  all[,i] <- all[,i] / sum(all[,i])
}
#ADD IN INTER-FAMILY once we have this.
rownames(all) <- c('inter_species','inter_genus','intra_species')
my_order <- c('intra_species','inter_species','inter_genus')
all <- all[match(my_order, rownames(all)),]

#Save output.----
saveRDS(all, output.path)

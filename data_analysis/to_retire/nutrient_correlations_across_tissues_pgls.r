#Do leaf tissue nutrient trait correlations differ by mycorrhizal type?
#The answer is no within element, and zero interactions across elements.
#Across elements they differ in ways you would expect based on overall mycorrhizal effects.x
rm(list=ls())
source('paths.r')
source('functions/pic_pro.r')
library(phytools)

#load data.---
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny.path)

#More data prep. Need to move this somewhere else.----
#mod tip labels
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
#add species rownames.
rownames(d) <- d$tpl.Species
d$Species <- d$tpl.Species
#code interactions for pgls.
d$MYCO_ASSO <- ifelse(d$MYCO_ASSO == 'ECM',1,0)
d$myc_ngreen <- d$MYCO_ASSO*d$Ngreen
d$myc_nroots <- d$MYCO_ASSO*d$Nroots
d$myc_nsenes <- d$MYCO_ASSO*d$Nsenes
d$myc_pgreen <- d$MYCO_ASSO*d$Pgreen
d$myc_psenes <- d$MYCO_ASSO*d$Psenes
d$myc_proots <- d$MYCO_ASSO*d$Proots
d$myc_log.ll <- d$MYCO_ASSO*d$log.ll
d$myc_rool.l <- d$MYCO_ASSO*d$root_lifespan

#Fit models.----
ng_nr <- pic_pro(y = 'Ngreen', x = c('Nroots','MYCO_ASSO','myc_nroots'),phylogeny = phy, trait.data = d)
ng_ns <- pic_pro(y = 'Ngreen', x = c('Nsenes','MYCO_ASSO','myc_nsenes'),phylogeny = phy, trait.data = d)
ns_nr <- pic_pro(y = 'Nsenes', x = c('Nroots','MYCO_ASSO','myc_nroots'),phylogeny = phy, trait.data = d)
pg_pr <- pic_pro(y = 'Pgreen', x = c('Proots','MYCO_ASSO','myc_proots'),phylogeny = phy, trait.data = d)
pg_ps <- pic_pro(y = 'Pgreen', x = c('Psenes','MYCO_ASSO','myc_psenes'),phylogeny = phy, trait.data = d)
ps_pr <- pic_pro(y = 'Psenes', x = c('Proots','MYCO_ASSO','myc_proots'),phylogeny = phy, trait.data = d)
ng_pg <- pic_pro(y = 'Ngreen', x = c('Pgreen','MYCO_ASSO','myc_pgreen'),phylogeny = phy, trait.data = d)
ng_ps <- pic_pro(y = 'Ngreen', x = c('Psenes','MYCO_ASSO','myc_psenes'),phylogeny = phy, trait.data = d)
ng_pr <- pic_pro(y = 'Ngreen', x = c('Proots','MYCO_ASSO','myc_proots'),phylogeny = phy, trait.data = d)
ns_ps <- pic_pro(y = 'Nsenes', x = c('Psenes','MYCO_ASSO','myc_psenes'),phylogeny = phy, trait.data = d)
ns_pr <- pic_pro(y = 'Nsenes', x = c('Proots','MYCO_ASSO','myc_proots'),phylogeny = phy, trait.data = d)
nr_pr <- pic_pro(y = 'Nroots', x = c('Proots','MYCO_ASSO','myc_proots'),phylogeny = phy, trait.data = d)
ns_pg <- pic_pro(y = 'Nsenes', x = c('Pgreen','MYCO_ASSO','myc_pgreen'),phylogeny = phy, trait.data = d)
nr_pg <- pic_pro(y = 'Nroots', x = c('Pgreen','MYCO_ASSO','myc_pgreen'),phylogeny = phy, trait.data = d)

#Summarize.----
ng_nr.s <- summary(ng_nr)
ng_ns.s <- summary(ng_ns)
ns_nr.s <- summary(ns_nr) #interactions! EM have more Ns than predicted by Nr, neg. interaction.
pg_pr.s <- summary(pg_pr)
pg_ps.s <- summary(pg_ps)
ps_pr.s <- summary(ps_pr)
ng_pg.s <- summary(ng_pg) #interactions! EM have less Ng than predicted by Pg, pos. interaction.
ng_ps.s <- summary(ng_ps)
ng_pr.s <- summary(ng_pr) #interactions! EM have more Ng than predicted by Pr, neg. interaction.
ns_ps.s <- summary(ns_ps)
ns_pr.s <- summary(ns_pr)
nr_pr.s <- summary(nr_pr)
ns_pg.s <- summary(ns_pg)
nr_pg.s <- summary(nr_pg)

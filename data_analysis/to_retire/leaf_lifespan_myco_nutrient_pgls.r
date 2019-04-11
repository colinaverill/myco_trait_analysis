#Do correlations between leaf lifespan and leaf nutrients differ by mycorrhizal type?
#yeah.
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
d$myc_log.ll <- d$MYCO_ASSO*d$log.LL
d$myc_rool.l <- d$MYCO_ASSO*d$root_lifespan
d$cols <- ifelse(d$MYCO_ASSO == 1,'purple','orange')

#Fit leaf lifespan models.----
ll_ng <- pic_pro(y = 'log.LL',x=c('Ngreen','MYCO_ASSO','myc_ngreen'), trait.data = d, phylogeny = phy)
ll_ns <- pic_pro(y = 'log.LL',x=c('Nsenes','MYCO_ASSO','myc_nsenes'), trait.data = d, phylogeny = phy)
ll_pg <- pic_pro(y = 'log.LL',x=c('Pgreen','MYCO_ASSO','myc_pgreen'), trait.data = d, phylogeny = phy)
ll_ps <- pic_pro(y = 'log.LL',x=c('Psenes','MYCO_ASSO','myc_psenes'), trait.data = d, phylogeny = phy)

#Summarize.----
ll_ng.s <- summary(ll_ng)
ll_ns.s <- summary(ll_ns)
ll_pg.s <- summary(ll_pg)
ll_ps.s <- summary(ll_ps)

plot(log.LL ~ Ngreen, data = d, pch = 16, col = d$cols)
abline(coef(ll_ng)[1] + coef(ll_ng)[3], coef(ll_ng)[2] + coef(ll_ng)[4], col = 'purple', lwd = 2)
abline(coef(ll_ng)[1], coef(ll_ng)[2], col = 'orange', lwd = 2)

plot(log.LL ~ Pgreen, data = d, pch = 16, col = d$cols)

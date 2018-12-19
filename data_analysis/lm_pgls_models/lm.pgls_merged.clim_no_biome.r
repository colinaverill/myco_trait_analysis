#fitting models with merged climate derived from gbif and then aggregated site observations.
#NO BIOME in this analysis.
#clear R environment, source functions and packages.
rm(list=ls())
library(data.table)
library(caper)
source('paths.r')
source('functions/pic_pro.r')
source('functions/report_myco_lm.r')
source('functions/report_myco_pgls.r')

#set output path.----
model.output.path <- lm_pgls_means_myc.pgf_merged.clim_models_NO_BIOME.path
summary.output.path <- lm_pgls_means_myc.pgf_merged.clim_summary_NO_BIOME.path

#load trait data and phylogeny.----
d <- data.table(readRDS(inter_specific_analysis_data.path))
phy <- read.tree(phylogeny_raw.path)

#Some data prep that should really be done somewhere else.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
rownames(d) <- d$Species
setnames(d,'root_lifespan','root.L')
d$biome2 <- ifelse(d$biome2 == 'Temperate_forest','aaa_Temperate_forest',d$biome2)

#Run lm models and summarize.----
Ngreen.lm <- lm(log10(Ngreen) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Nsenes.lm <- lm(log10(Nsenes) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Nroots.lm <- lm(log10(Nroots) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Pgreen.lm <- lm(log10(Pgreen) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Psenes.lm <- lm(log10(Psenes) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Proots.lm <- lm(log10(Proots) ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
log.LL.lm <- lm(log.LL        ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
root.L.lm <- lm(log10(root.L) ~ MYCO_ASSO + pgf        + mat.c + map.c, data = d) #zero nfixing root lifespan observations.
lm.output <- list(Ngreen.lm, Nsenes.lm, Nroots.lm, Pgreen.lm, Psenes.lm, Proots.lm, log.LL.lm, root.L.lm)
lm.sum <- list()
for(i in 1:length(lm.output)){
  lm.sum[[i]] <- report_myco_lm(lm.output[[i]])
}
lm.sum <- data.frame(do.call(rbind,lm.sum))

#Run PGLS analyses.----
Ngreen.pg <- pic_pro(y = 'Ngreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
Nsenes.pg <- pic_pro(y = 'Nsenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
Nroots.pg <- pic_pro(y = 'Nroots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
Pgreen.pg <- pic_pro(y = 'Pgreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
Psenes.pg <- pic_pro(y = 'Psenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
Proots.pg <- pic_pro(y = 'Proots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T)
log.LL.pg <- pic_pro(y = 'log.LL', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
root.L.pg <- pic_pro(y = 'root.L', x = c('MYCO_ASSO'       ,'pgf','mat.c','map.c'), phylogeny = phy, trait.data = d, log = T) #zero nfixing root lifespan observations.
pg.output <- list(Ngreen.pg, Nsenes.pg, Nroots.pg, Pgreen.pg, Psenes.pg, Proots.pg, log.LL.pg, root.L.pg)
pg.sum <- list()
for(i in 1:length(pg.output)){
  pg.sum[[i]] <- report_myco_pgls(pg.output[[i]], d)
}
pg.sum <- data.frame(do.call(rbind,pg.sum))

#bring it all together and save.----
#add lm or pg suffix to trait names.
pg.sum$trait <- paste0(pg.sum$trait,'.pg')
lm.sum$trait <- paste0(lm.sum$trait,'.lm')

#bind together summaries.
summaries <- rbind(lm.sum,pg.sum)
summaries[,2:ncol(summaries)] <- lapply(summaries[,2:ncol(summaries)],as.character)
summaries[,2:ncol(summaries)] <- lapply(summaries[,2:ncol(summaries)],as.numeric)

#Get together model list.
model.list <- c(lm.output,pg.output)
names(model.list) <- summaries$trait

#Final save lines.
saveRDS(summaries, summary.output.path)
saveRDS(model.list,model.output.path)

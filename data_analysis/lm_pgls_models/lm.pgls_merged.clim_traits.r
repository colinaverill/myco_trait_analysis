#fitting models with merged climate derived from gbif and then aggregated site observations.
#clear R environment, source functions and packages.
rm(list=ls())
library(data.table)
source('paths.r')
source('functions/pic_pro.r')
source('functions/wrap_results_lm.r')
source('functions/wrap_results_pg.r')

#set output path.----
model.output.path <- lm_pgls_means_myc.pgf_merged.clim_models.path
summary.output.path <- lm_pgls_means_myc.pgf_merged.clim_summary.path

#load trait data and phylogeny.----
d <- data.table(readRDS(inter_specific_analysis_data.path))
phy <- read.tree(phylogeny_raw.path)

#subset for testing
#d <- d[1:200,]

#Some data prep that should really be done somewhere else.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
rownames(d) <- d$Species
setnames(d,'root_lifespan','root.L')


#Run lm models and summarize.----
Ngreen.lm <- lm(Ngreen ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Nsenes.lm <- lm(Nsenes ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Nroots.lm <- lm(Nroots ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Pgreen.lm <- lm(Pgreen ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Psenes.lm <- lm(Psenes ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
Proots.lm <- lm(Proots ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
log.LL.lm <- lm(log.LL ~ MYCO_ASSO + pgf + nfix + mat.c + map.c, data = d)
root.L.lm <- lm(root.L ~ MYCO_ASSO + pgf        + mat.c + map.c, data = d) #zero nfixing root lifespan observations.
lm.output <- list(Ngreen.lm, Nsenes.lm, Nroots.lm, Pgreen.lm, Psenes.lm, Proots.lm, log.LL.lm, root.L.lm)
lm.sum <- list()
for(i in 1:length(lm.output)){
  lm.sum[[i]] <- wrap_results_lm(lm.output[[i]])
}
lm.sum <- data.frame(do.call(plyr::rbind.fill,lm.sum))

#Run PGLS analyses.----
Ngreen.pg <- pic_pro(y = 'Ngreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
Nsenes.pg <- pic_pro(y = 'Nsenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
Nroots.pg <- pic_pro(y = 'Nroots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
Pgreen.pg <- pic_pro(y = 'Pgreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
Psenes.pg <- pic_pro(y = 'Psenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
Proots.pg <- pic_pro(y = 'Proots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
log.LL.pg <- pic_pro(y = 'log.LL', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c'), phylogeny = phy, trait.data = d)
root.L.pg <- pic_pro(y = 'root.L', x = c('MYCO_ASSO'       ,'pgf','mat.c','map.c'), phylogeny = phy, trait.data = d) #zero nfixing root lifespan observations.
pg.output <- list(Ngreen.pg, Nsenes.pg, Nroots.pg, Pgreen.pg, Psenes.pg, Proots.pg, log.LL.pg, root.L.pg)
pg.sum <- list()
for(i in 1:length(pg.output)){
  pg.sum[[i]] <- wrap_results_pg(pg.output[[i]], d)
}
pg.sum <- data.frame(do.call(plyr::rbind.fill,pg.sum))

#bring it all together and save.----
summaries <- rbind(lm.sum,pg.sum)
rownames(summaries) <- summaries$model
model.list <- c(lm.output,pg.output)
names(model.list) <- summaries$model
saveRDS(summaries, summary.output.path)
saveRDS(model.list,model.output.path)

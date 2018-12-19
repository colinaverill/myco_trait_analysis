#testing for interaction between mycorrhizal traits and latitudinal zone as biome.
rm(list=ls())
library(data.table)
library(caper)
source('paths.r')
source('functions/report_myco_lm.r')
source('functions/pic_pro.r')

#set output paths.----
output.path <- lm_pgls_means_myc.biome3_interaction_models.path

#load data.----
d <- readRDS(inter_specific_analysis_data.path)
phy <- read.tree(phylogeny_raw.path)

#Some data prep.----
phy$tip.label <- paste0(toupper(substr(phy$tip.label, 1, 1)), substr(phy$tip.label, 2, nchar(phy$tip.label)))
phy$tip.label <- gsub('_',' ',phy$tip.label)
phy$node.label <- NULL
rownames(d) <- d$Species
d <- d[d$Species %in% phy$tip.label,] #drops 21 species.
rownames(d) <- d$Species
setnames(d,'root_lifespan','root.L')
d <- d[!is.na(d$biome3),]
d <- d[!is.na(d$MYCO_ASSO)]
#make interaction term.
d$biome3_myc <- paste0(d$biome3,d$MYCO_ASSO)
d <- d[!is.na(d$MYCO_ASSO) & !is.na(d$pgf) & !is.na(d$nfix) & 
         !is.na(d$mat.c) & !is.na(d$map.c) & 
         !is.na(d$biome3) & !is.na(d$biome3_myc),]
#calulate resorption.
d$Presor <- d$Pgreen - d$Psenes
d$Nresor <- d$Ngreen - d$Nsenes
d$Nresor <- ifelse(d$Nresor == 0, d$Nresor == 0.01, d$Nresor)
d$Presor <- ifelse(d$Presor == 0, d$Nresor == 0.01, d$Nresor)

#Run lm models and summarize.----
Ngreen.lm <- lm(log10(Ngreen) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Nsenes.lm <- lm(log10(Nsenes) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Nroots.lm <- lm(log10(Nroots) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Pgreen.lm <- lm(log10(Pgreen) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Psenes.lm <- lm(log10(Psenes) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Proots.lm <- lm(log10(Proots) ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
log.LL.lm <- lm(log.LL        ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
root.L.lm <- lm(log10(root.L) ~ MYCO_ASSO*biome3 + pgf        + mat.c + map.c, data = d) #zero nfixing root lifespan observations.
Nresor.lm <- lm(       Nresor ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
Presor.lm <- lm(       Presor ~ MYCO_ASSO*biome3 + pgf + nfix + mat.c + map.c, data = d)
lm.output <- list(Ngreen.lm, Nsenes.lm, Nroots.lm, Pgreen.lm, Psenes.lm, Proots.lm, log.LL.lm, root.L.lm,Nresor.lm,Presor.lm)

#Run PGLS analyses.----
Ngreen.pg <- pic_pro(y = 'Ngreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
Nsenes.pg <- pic_pro(y = 'Nsenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
Nroots.pg <- pic_pro(y = 'Nroots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
Pgreen.pg <- pic_pro(y = 'Pgreen', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
Psenes.pg <- pic_pro(y = 'Psenes', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
Proots.pg <- pic_pro(y = 'Proots', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d, log = T)
log.LL.pg <- pic_pro(y = 'log.LL', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d)          #already log10 transformed.
root.L.pg <- pic_pro(y = 'root.L', x = c('MYCO_ASSO'       ,'pgf','mat.c','map.c','biome3'), phylogeny = phy, trait.data = d, log = T) #zero nfixing root lifespan observations.
Nresor.pg <- pic_pro(y = 'Nresor', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d)
Presor.pg <- pic_pro(y = 'Presor', x = c('MYCO_ASSO','nfix','pgf','mat.c','map.c','biome3'),interaction = c('MYCO_ASSO*biome3'), phylogeny = phy, trait.data = d)
pg.output <- list(Ngreen.pg, Nsenes.pg, Nroots.pg, Pgreen.pg, Psenes.pg, Proots.pg, log.LL.pg, root.L.pg,Nresor.pg,Presor.pg)

#get list of output, name and save.----
model.list <- c(lm.output,pg.output)
traits <- c('Ngreen','Nsenes','Nroots','Pgreen','Psenes','Proots','log.LL','root.L','Nresor','Presor')
names(model.list) <- c(paste0(traits,'.lm'),c(paste0(traits,'.pg')))
saveRDS(model.list,output.path)

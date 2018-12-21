#testing lasso procedure.
rm(list=ls())
source('paths.r')
library(caper)
library(glmnet)

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


#Actualy working example that picks variables.
#subset data.
dat <- d[1:200,c('Ngreen','mat.c','map.c')]
dat <- dat[complete.cases(dat),]
#fit lasso.
glmnet1<-cv.glmnet(x = as.matrix(dat[,c('mat.c','map.c')]), dat$Ngreen,type.measure='mse')
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[!(variables %in% '(Intercept)')]
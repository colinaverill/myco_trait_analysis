#testing lasso procedure.
source('paths.r')
R.utils::sourceDirectory('functions')

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


#Grab predictors.----
d <- data.table::data.table(d)
preds <- d[,.(Ngreen,MYCO_ASSO,biome3, pgf, nfix, mat.c, map.c)]
preds <- preds[complete.cases(preds),]
y <- preds$Ngreen
x <- preds[,2:ncol(preds)]
x <- as.data.frame(x)
x[i] <- lapply(x[sapply(x, is.character)], as.factor) #convert character to factor vectors.
x.fac <- x[,sapply(x, is.factor)] #grab factors.
x.num <- x[,sapply(x, is.numeric)]
model.string <- paste(colnames(x.fac), collapse = '+')
x_train <- model.matrix(as.formula(model.string), x.fac)

#Find best model N green.
test <- glmnet::cv.glmnet(x = as.matrix(x.num), y = y, alpha = 1) 
c <- coef(test, s = "lambda.min", exact=T)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']


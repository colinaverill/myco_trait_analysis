#getting a table of parameter estimates and p-values.
rm(list=ls())
source('paths.r')
library(caper)

#set output path.----
output.path <- pgls_model_parameter_table.path

#get model list.----
d <- readRDS(lm_pgls_means_myc.pgf_merged.clim_models.path)
d <- d[9:16] #subset to phylogentic models

#summarize models.----
d <- lapply(d, summary)

#grab what you want from the first model.----
out <- d[[1]]$coefficients[,c(1,4)]
names <- paste(names(d)[1], c('par','p'), sep= '_')
colnames(out) <- names
link <- rownames(out)
out <- cbind(link,out)

#loop over the rest of the models and combine.----
for(i in 2:length(d)){
  to_add <- d[[i]]
  to_add <- to_add$coefficients[,c(1,4)]
  names <- paste(names(d)[i], c('par','p'), sep= '_')
  colnames(to_add) <- names
  link <- rownames(to_add)
  to_add <- cbind(link,to_add)
  out <- merge(out,to_add, by = 'link', all = T)
}

#set order of dataframe rows.----
to_order <- rownames(d[[1]]$coefficients)
out <- out[match(to_order, out$link),]
#save order, gsub will mess it up.
to_order <- out$Nroots.pg_par

#clean up names.----
out$link <- gsub('biome2','',out$link)
out$link <- gsub('mat.c','MAT',out$link)
out$link <- gsub('map.c','MAP',out$link)
out$link <- gsub('pgfgymno','gymnosperm',out$link)
out$link <- gsub('MYCO_ASSOECM','ECM',out$link)
colnames(out)[1] <- 'predictor'

#convert values to scientific notation.----
out[,2:ncol(out)] <- apply(out[,2:ncol(out)], 2, as.character)
out[,2:ncol(out)] <- apply(out[,2:ncol(out)], 2, as.numeric)
out[,2:ncol(out)] <- apply(out[,2:ncol(out)], 2, formatC, format = 'e', digits = 2)

#save output as .csv.----
write.csv(out,output.path)

#full tpl lookup. You never wanted to do this,but you are.
#There is a qsub script to run this in parallel.
#clear environment, load data.
rm(list=ls())
source('paths.r')
source('functions/tic_toc.r')
library(Taxonstand)
library(doParallel)

#set output path.----
output.path <- full_tpl_output.path

#load data.----
d <- readRDS(tpl_names_lookup.path)

#cross reference against names you already looked up.----
if(file.exists(full_tpl_output.path)){
  ref <- readRDS(full_tpl_output.path)
  d <- d[!Species %in% ref$Taxon,]
}

testing = F
if(testing == T){
  d <- d[1:100,]
}

#register parallel environment.----
n.cores <- detectCores()
registerDoParallel(n.cores)

#setup a parallel lookup.
tic()
output.list <-
foreach(i = 1:nrow(d)) %dopar% {
  lookup <- TPL(d$Species[i])
  return(lookup)
}
toc()
out <- do.call(rbind, output.list)

#If you have already run this once, merge this into our existing assignments.----
if(file.exists(full_tpl_output.path)){
  out <- rbind(out,ref)
}

#Save output.----
saveRDS(out, output.path)

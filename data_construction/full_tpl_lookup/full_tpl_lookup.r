#full tpl lookup. You never wanted to do this,but you are.
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

#Save output
saveRDS(out, output.path)
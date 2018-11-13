#cross_ref_tpl.r
#Checks a datafile of species names and tpl names, cross references to a second data file, and updates.
#look up depends on data.table being loaded in the environment.
library(data.table)
cross_ref_tpl <- function(d1, d2.path){
  d1 <- data.table(d1)
  if(file.exists(d2.path)){
     check <- data.table(readRDS(d2.path))
    update <- d1[Species %in% check$Species,.(Species)]
    update <- merge(update, check, all.x = T)
    update <- rbind(update, d1[!(Species %in% check$Species),])
    return(update)    
  }
  if(!file.exists(d2.path)){
    cat('Reference file does not exist, returning original product.')
    return(d1)
  }
}
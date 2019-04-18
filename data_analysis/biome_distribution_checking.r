#counting biome3 observations.
rm(list=ls())
source('paths.r')
test <- readRDS(intra_specific_analysis_data.path)
test <- test[!is.na(test$latitude),]
spp <- unique(test$tpl.Species)
d <- read.csv('figures/Supplementary_Data_File_1.csv')
spp <- spp[spp %in% d$species]

check <- list()
for(i in 1:length(spp)){
  n.bore <- nrow(test[test$tpl.Species == spp[i] & test$biome_name3 == 'c_boreal',])
  n.temp <- nrow(test[test$tpl.Species == spp[i] & test$biome_name3 == 'a_temperate',])
  n.trop <- nrow(test[test$tpl.Species == spp[i] & test$biome_name3 == 'b_tropical',])
  return <- c(n.bore, n.temp, n.trop)
  check[[i]] <- return
}
check <- data.frame(do.call(rbind, check))
colnames(check) <- c('bore','temp','trop')
check$spp <- spp

test1 <- check[check$bore == check$temp,]
test1 <- test1[test1$bore > 0,]
test1 <- test1[!(test1$trop > test1$temp),]
test2 <- check[check$temp == check$trop,]
test2 <- test2[test2$trop > 0,]
test2 <- test2[!(test2$bore > test2$temp),]


#ORNL species woodiness harmonization.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- ornl_wood_clean.path

#load data.----
ornl <- data.table(read.csv(ornl_raw.path))

#grab columns of interest, assign doi.----
ornl <- ornl[!is.na(Growth_habit), .(Species, Growth_habit)]
ornl[,doi:='10.3334/ORNLDAAC/1106']

#Harmonize woodiness assignments.----
ornl[Growth_habit %in% c('Tree (deciduous)','Tree (evergreen)','Trees','Tree (evergreen) Liana','Conifers'),woody := 1]
ornl[Growth_habit %in% c('Graminoid','Forb','Ferns'),woody := 0]
setkey(ornl,'Species')
ornl <- unique(ornl,by = 'Species')
ornl[woody == 1, woodiness := 'W']
ornl[woody == 0, woodiness := 'H']
ornl <- ornl[!is.na(woodiness),.(Species,woodiness,doi)]

#Save output.----
saveRDS(ornl, output.path)

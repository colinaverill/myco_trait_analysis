#GLOP species woodiness harmonization.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- glop_wood_clean.path

#load data.----
glop <- data.table(read.csv(glop_raw.path))


#Assign doi, grab columns of interest.----
glop <- glop[!is.na(GF),.(Species, GF)]
glop[,doi:='10.1038/nature02403']

#Harmonize woodiness assignments.----
glop[GF %in% c('H','G','F','V'), woody := 0]
glop[GF %in% c('S','T')        , woody := 1]
setkey(glop,'Species')
glop <- unique(glop, by = 'Species')
glop[woody == 1, woodiness := 'W']
glop[woody == 0, woodiness := 'H']
glop <- glop[!is.na(woodiness),.(Species,woodiness,doi)]

#Save output.----
saveRDS(glop,output.path)

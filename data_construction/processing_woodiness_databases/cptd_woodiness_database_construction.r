#Processing woodiness data from FRED 2.0
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- cptd_wood_clean.path

#load data.----
cptd <- data.table(read.csv(cptd_PGF_raw.path))
names <- read.csv(cptd_names.path)
cptd <- merge(cptd, names, all = T)


#Assign species and doi, grab colums of interest.----
cptd[,Species := paste(ACCEPTED.GENUS,ACCEPTED.SPECIES, sep = ' ')]
cptd[,doi:='10.1002/ecy.2091']

#Harmonize woodiness assignments.----
cptd <- cptd[!is.na(Life.form),.(Species,Life.form,doi)]
cptd$Life.form <- as.character(cptd$Life.form)
cptd[Life.form %in% c('forb','graminoid','geophyte','pteridophyte','rosette forb'), woody := 0]
cptd[Life.form %in% c('tree','low to high shrub','erect dwarf shrub','liana','bamboo','prstrate dwarf shrub','small tree'), woody := 1]
#make sure everything is unique
setkey(cptd,'Species')
cptd <- unique(cptd, by = 'Species')
cptd[woody ==1, woodiness := 'W']
cptd[woody ==0, woodiness := 'H']
cptd <- cptd[!is.na(woodiness)]
cptd <- cptd[,.(Species,woodiness,doi)]

#Save output.----
saveRDS(cptd, output.path)

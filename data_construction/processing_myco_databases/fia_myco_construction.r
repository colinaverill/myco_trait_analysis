#FRED mycorrhizal data construction.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- fia_myco_clean.path

#load data.----
fia <- data.table(read.csv(fia_myco_raw.path))

###FIA 1183 unique mycorrhizal assignments.----
#grab columns of interest.----
fia$doi <- fia$ref
fia <- fia[,.(GENUS,SPECIES,MYCO_ASSO,doi)]
fia$Species <- paste(fia$GENUS,fia$SPECIES)

#harmonize mycorrhizal assignments.----
fia[MYCO_ASSO == 'EITHER', MYCO_ASSO := 'AM_ECM']
fia <- fia[!(MYCO_ASSO %in% c('UNK'))]

#grab unique species.----
fia$MYCO_ASSO <- as.character(fia$MYCO_ASSO)
fia <- fia[,.(Species,MYCO_ASSO,doi)]
setkey(fia, Species)
fia <- unique(fia, by = 'Species')

#Save output.----
saveRDS(fia,output.path)

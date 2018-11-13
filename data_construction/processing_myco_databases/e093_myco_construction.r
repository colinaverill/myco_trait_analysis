#e093 mycorrhizal data construction.
#DOI: 10.1890/11-1749.1
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <-e093_myco_clean.path

#load data.----
e093 <- data.table(read.csv(e093_myco_raw.path))

#add doi, grab columns of interest.----
e093$doi <- rep('10.1890/11-1749.1', nrow(e093))
e093 <- e093[,.(Species, Modern.term.for.type.of.mycorrhiza,doi)]
setnames(e093,old=c('Modern.term.for.type.of.mycorrhiza'),new=c('MYCO_ASSO'))

#harmonize the mycorrhizal classifications.
e093$MYCO_ASSO <- as.character(e093$MYCO_ASSO)
e093[MYCO_ASSO == 'Ectomycorrhiza ', MYCO_ASSO := 'ECM']
e093[MYCO_ASSO == 'Orchid'         , MYCO_ASSO := 'ORM']
e093[MYCO_ASSO == 'Ericoid'        , MYCO_ASSO := 'ERM']
e093[MYCO_ASSO == 'Arbutoid'       , MYCO_ASSO := 'ABM']
e093 <- e093[!(MYCO_ASSO %in% c('','Endo-unidentified','no'))]

#Grab all unique rows
e093 <- unique(e093)

#Some species have multiple mycorrhizal associations. Every one of these cases is a dual AM ECM situation.
duped <- e093[Species %in% e093[duplicated(e093$Species),]$Species,]
e093[Species %in% duped$Species, MYCO_ASSO := 'AM_ECM']
setkey(e093, Species)
e093 <- unique(e093, by = 'Species')

#save output.----
saveRDS(e093,output.path)

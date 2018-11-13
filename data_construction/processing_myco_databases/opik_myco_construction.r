###opik - Bueno et al. 2017 GEB paper from University of Tartu. 1441 assignments, 614 Species not in mflo database!
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <-opik_myco_clean.path

#load data.----
opik <- data.table(read.csv(opik_myco_raw.path))

#assign doi, grab columns of interest.----
opik$doi <- rep('10.1111/geb.12582', nrow(opik))
opik <- opik[,.(Species..taken.from.The.Plant.List.,Type,doi)]
setnames(opik, old=c('Species..taken.from.The.Plant.List.','Type'),new=c('Species','MYCO_ASSO'))
opik$MYCO_ASSO <- as.character(opik$MYCO_ASSO)

#Harmonize mycorrhizal assignments.----
opik[MYCO_ASSO == 'AM+NM'          , MYCO_ASSO := 'AM'    ]
opik[MYCO_ASSO == 'ECM+NM'         , MYCO_ASSO := 'ECM'   ]
opik[MYCO_ASSO == 'AM+ECM'         , MYCO_ASSO := 'AM_ECM']
opik[MYCO_ASSO == 'AM+ECM+NM'      , MYCO_ASSO := 'AM_ECM']
opik[MYCO_ASSO == 'ABM+NM'         , MYCO_ASSO := 'ABM'   ]
opik <- opik[!(MYCO_ASSO %in% c('MTM+ECM',''))]

#Save output.----
saveRDS(opik, output.path)

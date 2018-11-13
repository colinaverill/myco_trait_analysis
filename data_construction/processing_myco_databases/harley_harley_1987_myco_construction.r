#Harley and Harley 1987, digitized by SNK.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <-harley_harley_myco_clean.path

#load data.----
harl <- data.table(read.csv(harley_harley_myco_raw.path))

#set doi, grab columns of interest.----
harl$doi <- rep('10.1111/j.1469-8137.1987.tb00674.x',nrow(harl))
harl <- harl[,.(X,Mycorrhizal.type,doi)]
setnames(harl,c('X','Mycorrhizal.type'),c('Species','MYCO_ASSO'))
harl$MYCO_ASSO <- as.character(harl$MYCO_ASSO)

#Harmonize mycorrhizal assignments.----
harl[MYCO_ASSO == 'EITHER'            , MYCO_ASSO := 'AM_ECM'    ]
harl[MYCO_ASSO == 'ERM and AM and ECM', MYCO_ASSO := 'AM_ECM_ERM']
harl[MYCO_ASSO == 'AM and ERM'        , MYCO_ASSO := 'AM_ERM'    ]

#Save output.----
saveRDS(harl,output.path)

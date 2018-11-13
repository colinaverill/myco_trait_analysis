#Mycoflor mycorrhizal data construction.
#DOI: 10.1890/12-1700.1
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <-mycoflor_myco_clean.path

#load data.----
mflo <- data.table(read.csv(mycoflor_myco_raw.path))

#set doi, grab columns of interest.----
mflo$doi <- rep('10.1890/12-1700.1', nrow(mflo))
mflo <- mflo[,.(Plant.species,Mycorrhizal.status,doi)]
setnames(mflo, old=c('Plant.species','Mycorrhizal.status'), new=c('Species','MYCO_ASSO'))
mflo$MYCO_ASSO <- as.character(mflo$MYCO_ASSO)

#Harmonize mycorrhizal assignments.----
#If things are listed as mycorrhizal and not mycorrhizal, dropping the NM designation.
mflo[MYCO_ASSO == 'AM+NM'          , MYCO_ASSO := 'AM']
mflo[MYCO_ASSO == 'AM+ECM'         , MYCO_ASSO := 'AM_ECM']
mflo[MYCO_ASSO == 'ECM+AM'         , MYCO_ASSO := 'AM_ECM']
mflo[MYCO_ASSO == 'AM+ECM+NM'      , MYCO_ASSO := 'AM_ECM']
mflo[MYCO_ASSO == 'AM+ECM+EEM+NM'  , MYCO_ASSO := 'AM_ECM_EEM']
mflo[MYCO_ASSO == 'ECM+EEM'        , MYCO_ASSO := 'ECM_EEM']
mflo[MYCO_ASSO == 'ECM+EEM+AM'     , MYCO_ASSO := 'AM_ECM_EEM']
mflo[MYCO_ASSO == 'AM+ECM+EEM'     , MYCO_ASSO := 'AM_ECM_EEM']
mflo[MYCO_ASSO == 'AM+NM+ERM'      , MYCO_ASSO := 'AM_ERM']
mflo[MYCO_ASSO == 'AM+ORM'         , MYCO_ASSO := 'AM_ORM']
mflo[MYCO_ASSO == 'ERM+ABM'        , MYCO_ASSO := 'ABM_ERM']
mflo[MYCO_ASSO == 'ERM+ABM+ECM+EEM', MYCO_ASSO := 'ABM_ECM_ERM_EEM']
mflo[MYCO_ASSO == 'ERM+NM'         , MYCO_ASSO := 'ERM']
mflo[MYCO_ASSO == 'ECM+NM'         , MYCO_ASSO := 'ECM']
mflo[MYCO_ASSO == 'ORM+ECM'        , MYCO_ASSO := 'ECM_ORM']
mflo[MYCO_ASSO == 'ORM+ECM+AM'     , MYCO_ASSO := 'AM_ECM_ORM']
mflo[MYCO_ASSO == 'ORM+NM'         , MYCO_ASSO := 'ORM']
mflo[MYCO_ASSO == 'ORM+ECM+NM'     , MYCO_ASSO := 'ECM_ORM']
mflo[MYCO_ASSO == 'AM+ORM+ECM'     , MYCO_ASSO := 'AM_ECM_ORM']
mflo[MYCO_ASSO == 'ORM+AM'         , MYCO_ASSO := 'AM_ORM']
mflo[MYCO_ASSO == 'ABM+NM'         , MYCO_ASSO := 'ABM']
mflo[MYCO_ASSO == 'ECM+EEM+NM'     , MYCO_ASSO := 'ECM_EEM']
mflo[MYCO_ASSO == 'ERM+ECM+ABM'    , MYCO_ASSO := 'ABM_ECM_ERM']
mflo[MYCO_ASSO == 'EEM+ERM+ECM+ABM', MYCO_ASSO := 'ABM_ECM_ERM_EEM']
mflo[MYCO_ASSO == 'AM+NM+ECM'      , MYCO_ASSO := 'AM_ECM']
mflo[MYCO_ASSO == 'ECM+AM+NM'      , MYCO_ASSO := 'AM_ECM']
mflo <- mflo[!(MYCO_ASSO %in% c('M(endo)+ABM','EEM+M(endo)+NM+ABM','EEM+M(endo)+ABM','MTM+ECM+AM',"AM+M(endo)+NM",'M(endo)','Mycoheterotrophy'))]

#grab only unique AM and ECM species.----
mflo <- mflo[MYCO_ASSO %in% c('AM','ECM','AM_ECM','ORM','ABM','ERM','ECM_ERM','AM_ERM')]
#Two duplicated species entries. No conflicting mycorrhizal assignments.
duped <- mflo[Species %in% mflo[duplicated(mflo$Species),]$Species,]
setkey(mflo,'Species')
mflo <- unique(mflo, by = 'Species')

#Save output.----
saveRDS(mflo,output.path)

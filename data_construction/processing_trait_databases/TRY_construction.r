#organizing TRY data request.
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- try_intra.path
inter.output.path <- try_inter.path

#load data.----
try <- data.table(read.csv(try_raw.path))

#Grab traits of interest.----
try.Ngreen <- try[DataID == 15,]
try.Pgreen <- try[DataID == 16,]
try.Nsenes <- try[DataID == 249,]
try.Psenes <- try[DataID == 250,]
try.log.LL <- try[DataID == 13,]
try.lat    <- try[DataID == 59,]
try.lon    <- try[DataID == 60,]
setnames(try.Ngreen,'StdValue','Ngreen')
setnames(try.Pgreen,'StdValue','Pgreen')
setnames(try.Nsenes,'StdValue','Nsenes')
setnames(try.Psenes,'StdValue','Psenes')
setnames(try.log.LL,'StdValue','log.LL')
setnames(try.lat   ,'StdValue','latitude')
setnames(try.lon   ,'StdValue','longitude')
#convert try LL to log.LL (log base 10) in months to be consistent with units of other datasets.
try.log.LL$log.LL <- log(try.log.LL$log.LL, base = 10)

#Build intra-specific product.----
try.intra <- try.Ngreen[,.(SpeciesName,ObservationID,Ngreen)]
try.intra <- merge(try.intra,try.Pgreen[,.(SpeciesName,ObservationID,Pgreen)], by = c('SpeciesName','ObservationID'), all = T)
try.intra <- merge(try.intra,try.Nsenes[,.(SpeciesName,ObservationID,Nsenes)], by = c('SpeciesName','ObservationID'), all = T)
try.intra <- merge(try.intra,try.Psenes[,.(SpeciesName,ObservationID,Psenes)], by = c('SpeciesName','ObservationID'), all = T)
try.intra <- merge(try.intra,try.log.LL[,.(SpeciesName,ObservationID,log.LL)], by = c('SpeciesName','ObservationID'), all = T)
try.intra <- merge(try.intra,try.lat [,.(SpeciesName,ObservationID,latitude)], by = c('SpeciesName','ObservationID'), all = T)
try.intra <- merge(try.intra,try.lon[,.(SpeciesName,ObservationID,longitude)], by = c('SpeciesName','ObservationID'), all = T)
try.intra$source <- rep('TRY_database',nrow(try.intra))
try.intra$doi    <- rep('10.1111/j.1365-2486.2011.02451.x', nrow(try.intra))
setnames(try.intra,'SpeciesName','Species')
try.intra$ObservationID <- NULL

#aggregate individual TRY datasets by species.----
try.inter <- try.intra[, lapply(.SD, mean, na.rm = T), by=Species, .SDcols=c('Ngreen','Pgreen','Nsenes','Psenes','log.LL')]
try.inter$source <- rep('TRY_database',nrow(try.inter))
try.inter$doi    <- rep('10.1111/j.1365-2486.2011.02451.x', nrow(try.inter))
try.inter[is.na(try.inter)] = NA

#Save output.----
saveRDS(try.intra, intra.output.path)
saveRDS(try.inter, inter.output.path)

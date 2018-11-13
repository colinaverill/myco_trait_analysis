#organizing glop data.
##Note log.LL is log base 10 (months).
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- glop_intra.path
inter.output.path <- glop_inter.path

#load data.----
glop <- data.table(read.csv(glop_raw.path))

#dial in names and sources.----
glop$Species <- trimws(glop$Species, which = 'both') #trim leading/trailing white space
glop$source <- rep('glop', nrow(glop))
glop$doi <- rep('10.1038/nature02403', nrow(glop))

#grab columns of interest.----
glop <- glop[,.(Species,GF,log.LL,log.Nmass,log.Pmass,source,doi)]

#Convert element concentrations to mg/g.----
glop$Nmass <- (10^glop$log.Nmass) * 10
glop$Pmass <- (10^glop$log.Pmass) * 10
glop$Nmass <- round(glop$Nmass,2)
glop$Pmass <- round(glop$Pmass,2)

#Get woody or not based on growth form.----
glop[,woody := GF %in% c('T','S')+0L]

#get rid of lianas and vines.-----
glop <- glop[!(GF == 'V'),]

#change names.----
setnames(glop,c('Nmass','Pmass'),c('Ngreen','Pgreen'))

#Dial in intra-specific product.
glop.intra.out <- glop[,.(Species,Ngreen,Pgreen,log.LL,source,doi)]

#Get interspecific means.----
z <- glop[, lapply(.SD, mean, na.rm = T), by=Species, .SDcols=c('Ngreen','Pgreen','log.LL')]
setkey(glop,'Species')
glop <- unique(glop, by = 'Species')
glop <- merge(z,glop[,.(Species,woody,source,doi)], by = 'Species')
#convert NaN values to NA
glop[sapply(glop,is.na)] = NA 
glop.inter.out <- glop

#save output.----
saveRDS(glop.intra.out, intra.output.path)
saveRDS(glop.inter.out, inter.output.path)

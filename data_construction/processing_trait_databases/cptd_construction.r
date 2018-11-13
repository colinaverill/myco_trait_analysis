#organizing Chinest Plant Trait Database (cptd).
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- cptd_intra.path
inter.output.path <- cptd_inter.path

#load data.----
cptd <- data.table(read.csv(cptd_traits_raw.path))
cptd.names <- read.csv(cptd_names.path)
cptd.sites <- read.csv(cptd_sites_raw.path)

###CPTD - 1,193 species level observations - Chinese Plant Trait Database. Over 1k of these not in either of the other foliar databases.
#merge names, assign species, dial in sources.----
cptd <- merge(cptd,cptd.names, by = 'SAMPLE.ID')
cptd <- merge(cptd,cptd.sites, by = 'Site.ID')
cptd$Species <- paste(cptd$ACCEPTED.GENUS,cptd$ACCEPTED.SPECIES, sep = ' ')
cptd$source <- rep('cptd',nrow(cptd))
cptd$doi    <- rep('10.1002/ecy.2091',nrow(cptd))

#subset to columns of interest, dial in names and units.----
cptd <- cptd[,.(Species,Nmass..g.kg.,Pmass..g.kg.,Latitude,Longitude,source,doi)]
setnames(cptd,c('Nmass..g.kg.','Pmass..g.kg.','Latitude','Longitude'),c('Ngreen','Pgreen','latitude','longitude'))

#save intra-specific output.----
cptd.intra.out <- cptd[,.(Species,Ngreen,Pgreen,latitude,longitude,source,doi)]

#Get interspecific means.----
z <- cptd[,lapply(.SD, mean, na.rm=T), by = Species, .SDcols=c('Ngreen','Pgreen')]
setkey(cptd,'Species')
cptd <- unique(cptd, by = 'Species')
cptd <- merge(z,cptd[,.(Species,source,doi)], by = 'Species')
#convert NaN values to NA
cptd[sapply(cptd,is.na)] = NA 
cptd.inter.out <- cptd

#Save output.----
saveRDS(cptd.intra.out,intra.output.path)
saveRDS(cptd.inter.out,inter.output.path)

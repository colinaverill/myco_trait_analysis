#organizing ORNL data.
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- ornl_intra.path
inter.output.path <- ornl_inter.path

#load data.----
ornl <- data.table(read.csv(ornl_raw.path))

###ORNL - 649 species level observations.
#add source data.----
ornl$source <- rep('Vergutz et al.', nrow(ornl))
ornl$doi <- rep('10.3334/ORNLDAAC/1106', nrow(ornl))
ornl$Species <- trimws(ornl$Species, which = 'both') #trim white space

#remove lianas and vines----
ornl <- ornl[!(Growth_habit =='Tree (evergreen) Liana'),]

#rename some variables an subset to interesting ones.----
setnames(ornl,c('N_green_leaf','P_green_leaf','N_senesced_leaf','P_senesced_leaf','Lat','Long'),
         c('Ngreen','Pgreen','Nsenes','Psenes','latitude','longitude'))
ornl <- ornl[,.(Species,Ngreen,Pgreen,Nsenes,Psenes,latitude,longitude,source,doi)]

#convert element percentages to mg / g.----
ornl$Ngreen <- ornl$Ngreen * 10
ornl$Pgreen <- ornl$Pgreen * 10
ornl$Nsenes <- ornl$Nsenes * 10
ornl$Psenes <- ornl$Psenes * 10

#save intra-specific output.
ornl.intra.out <- ornl

#Get species level averages.----
z <- ornl[, lapply(.SD, mean, na.rm = T), by = Species, .SDcols=c('Ngreen','Pgreen','Nsenes','Psenes')]
setkey(ornl,'Species')
ornl <- unique(ornl, by = 'Species')
ornl <- merge(z,ornl[,.(Species,source,doi)], by = 'Species')
#convert NaN values to NA
ornl[sapply(ornl,is.na)] = NA 
ornl.inter.out <- ornl

#save output.----
saveRDS(ornl.intra.out, intra.output.path)
saveRDS(ornl.inter.out, inter.output.path)


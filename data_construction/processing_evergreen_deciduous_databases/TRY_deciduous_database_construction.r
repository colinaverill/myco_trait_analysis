#Try categorical traits table with deciduousness information - passed along from Will Pearse.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- try_decid_clean.path

#load raw data.----
d <- read.csv(try_decid_raw.path)

#Format data set.----
#grab columns of interest
d <- d[,c('AccSpeciesName','LeafPhenology')]

#Rename columns.
colnames(d) <- c('Species','deciduous')
d$deciduous <- as.character(d$deciduous)

#drop species with no deciduous entry.
d <- d[!(d$deciduous == ''),]
#drop species with deciduous/evergreen entry.
d <- d[!(d$deciduous == 'deciduous/evergreen'),]

#Set deciduous to 0-1. 0 if evergreen, 1 if deciduous.
d$deciduous <- ifelse(d$deciduous == 'deciduous',1,0)

#Make sure species is a character, not a factor vector.
d$Species <- as.character(d$Species)

#Save output.----
saveRDS(d,output.path)
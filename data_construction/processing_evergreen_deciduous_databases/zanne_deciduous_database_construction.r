#Zanne et al. 2014 Nature- evergreen deciduous data harmonization.
rm(list=ls())
source('paths.r')

#set output path.----
output.path <- zanne_decid_clean.path

#load raw data.----
d <- read.csv(zanne_decid_raw.path)

#Format data set.----
#Rename columns.
colnames(d) <- c('Species','deciduous')

#Set deciduous to 0-1. 0 if evergreen, 1 if deciduous.
d$deciduous <- ifelse(d$deciduous == 'D',1,0)

#Make sure species is a character, not a factor vector.
d$Species <- as.character(d$Species)

#Save output.----
saveRDS(d,output.path)
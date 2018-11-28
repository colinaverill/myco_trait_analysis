#processing non-TRY data rom Keller and Philliops 2018 New Phytologist. doi: 10.1111/nph.15524
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- kell_phil2018_clean.path

#load data.----
d <- data.table(read.csv(kell_phil_2018_raw.path))

#subset to columns of interest, set names, add doi.----
d <- d[,.(AccSpeciesName,Lat_DD,Long_DD,LitterN,LitterP)]
colnames(d) <- c('Species','latitude','longitude','Nsenes','Psenes')
#add doi
d$doi <- '10.1111/nph.15524'

#save output.----
saveRDS(d, output.path)
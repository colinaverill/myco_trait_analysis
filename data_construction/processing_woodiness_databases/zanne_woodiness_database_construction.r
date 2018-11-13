#Processing woodiness data from Zanne et al.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- zann_wood_clean.path

#load data.----
wh <- data.table(read.csv(zann_raw.path))

#assign doi, grab columns of interest.----
wh[,doi:='10.5061/dryad.63q27/2']
wh <- wh[woodiness %in% c('H','W')]
setnames(wh,c('gs'),c('Species'))
wh <- wh[,.(Species,woodiness,doi)]

#save output.----
saveRDS(wh, output.path)
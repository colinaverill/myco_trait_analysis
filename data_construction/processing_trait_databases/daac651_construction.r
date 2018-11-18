#ORNL DAAC litter chemistry data from elevated CO2 experiments. Use controls.
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.----
output.path <- daac651_intra.path

#load data.----
d <- data.table(read.table(daac651_raw.path,sep ='\t', header = T))
setnames(d,c('SPECIES','N_CONC_CON'),c('Species','Nsenes'))

#grab stuff from field conditions, unfertilized.----
d <- d[FIELD_POT == 'field' & FERTILIZER == 'no',]
d$doi <- '10.3334/ORNLDAAC/651'

#grab columns of interest.----
to_return <- d[,.(Species,Nsenes,doi)]

#save output.----
saveRDS(to_return,output.path)

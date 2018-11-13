#miatto et al. 2016 Brazilian Tree Species.
#NOTE: only have species-level means, no intra-specific.
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- miat_intra.path
inter.output.path <- miat_inter.path

#load data.----
miat <- data.table(read.csv(miat_raw.path))

#Add source info, subset to columns of interest.
miat$source <- rep('miat',nrow(miat))
miat$doi    <- rep('10.1007/s11104-016-2796-2',nrow(miat))
miat <- miat[,.(Species, Ngreen, Pgreen, Nsenes, Psenes, source, doi)]
miat$Species <- as.character(miat$Species)

#Save output.
saveRDS(miat, inter.output.path)

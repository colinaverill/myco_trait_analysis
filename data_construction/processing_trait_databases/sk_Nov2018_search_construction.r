#Stehphanie Kivlin's literature for more root traits and leaf lifespan conducted November 2018.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- sk_nov2018_clean.path

#load data.----
d <- read.csv(sk_nov2018_raw.path)

#flip factors to character vectors.----
d$N_roots_doi <- as.character(d$N_roots_doi)
d$log.LL_doi <- as.character(d$log.LL_doi)
d$root_lifespan_doi <- as.character(d$root_lifespan_doi)
d$log.LL_doi <- ifelse(d$log.LL_doi=='',NA,d$log.LL_doi)
d$N_roots_doi <- ifelse(d$N_roots_doi=='',NA,d$N_roots_doi)


#merge dois.----
d$doi <- d$root_lifespan_doi
d$doi <- ifelse(!is.na(d$log.LL_doi), d$log.LL_doi, d$doi)
d$doi <- ifelse(!is.na(d$N_roots_doi), d$N_roots_doi, d$doi)

#clean up columns.----
d$N_roots_doi <- NULL
d$log.LL_doi <- NULL
d$root_lifespan_doi <- NULL

#save output.----
saveRDS(d, output.path)

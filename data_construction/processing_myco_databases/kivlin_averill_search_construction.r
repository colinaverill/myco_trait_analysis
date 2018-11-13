#Kivlin and Averill literature search for additional Mycorrhizal assignments.
#original source dois provided, when available.
rm(list=ls())
rm(list=ls())
source('paths.r')
library(data.table)
library(googlesheets)
library(openssl)

#set output path.----
output.path <- averill_kivlin_myco_clean.path

#load google sheet.----
#specify googlesheet url
new.g.url <- 'https://docs.google.com/spreadsheets/d/1EJ4ndiFbZjd0n16fCEkjq1jnAErrWXMT2AtvfPfsVyw/edit#gid=194275577'
#remote sheet name
remote_sheet_name <- 'hand checking mycorrhizal assignments'
#QWhere to download sheet to.
remote.dl.path <- '/fs/data3/caverill/myc_traits/remote_traits_download.csv'

#'register' and download remote google sheet.
google_data_url <- gs_url(new.g.url)
gs_download(google_data_url, ws = NULL, to = remote.dl.path, overwrite = T, verbose = TRUE)

#load it into memory.
z <- data.table(read.csv(remote.dl.path))
z <- z[,-1] #get rid of column that gets added on upload
z$Species <- as.character(z$Species)
setnames(z,'MYCO_ASS0_new','MYCO_ASSO_new')
z <- z[,.(Species, tpl.Species,Species_new,MYCO_ASSO_new,myco_doi_new,to_delete)]

#Subset to new entries, update names.----
z <- z[  is.na(to_delete),]
z <- z[ !is.na(MYCO_ASSO_new),]
z <- z[!(is.na(myco_doi_new)),]
z[!is.na(Species_new), Species := Species_new]

#Grab columns of interest, update column names.----
z <- z[,.(Species,MYCO_ASSO_new,myco_doi_new)]
setnames(z,c('MYCO_ASSO_new','myco_doi_new'),c('MYCO_ASSO','doi'))
#make everything character.
z[,] <- lapply(z, as.character)

#save output.----
saveRDS(z,output.path)

#names have been hand-checked to fix many errors. Update files to reflect this.
rm(list=ls())
source('paths.r')
library(data.table)
library(googlesheets)
library(openssl)

#set output path.----
output.path <- merged_intra_traits_names_hand_checked.path
#load intra-specific data.----
d <- readRDS(merged_intra_traits.path)
#load data in google sheet.----
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
z <- z[,.(Species, tpl.Species,Species_new,to_delete)]


#Delete species that need to be deleted.----
to_remove <- z[to_delete == 1,]$Species
d <- d[!(Species %in% to_remove),]

#Updated Species names as appropriate.----
  keep <- d[!(Species %in% z[!is.na(Species_new)]$Species),] #observations that do not need updated species names.
update <- d[  Species %in% z[!is.na(Species_new)]$Species, ] #observations that do need updated species names.
update <- merge(update, z[!is.na(Species_new), .(Species,Species_new)], all.x = T)
update$Species <- NULL
setnames(update,'Species_new','Species')
#merge the two subset back together.
d <- plyr::rbind.fill(keep,update)

#Save output.----
saveRDS(d, output.path)

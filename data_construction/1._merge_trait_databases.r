#Aggregating plant trait data to species across data sets.
#Removing observations for species in TRY that are present in other data sets.
#All element concentrations should be in mg / g. LL should be in log(months).
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)
library(Taxonstand)

#set output path.
intra.output.path <- merged_intra_traits.path
inter.output.path <- merged_inter_traits.path

#load cleaned up trait files.----
glop <- readRDS(glop_intra.path)
fred <- readRDS(fred_2.0_intra.path)
ornl <- readRDS(ornl_intra.path)
try <- readRDS( try_intra.path)
cptd <- readRDS(cptd_intra.path)
miat <- readRDS(miat_inter.path) #no intra-specific opportunity
bien <- readRDS(bien_intra.path)

#merge all intra-specific observaitons.----
traits <- plyr::rbind.fill(glop,fred,ornl,cptd,miat,bien,try)
traits$unique <- seq(1:nrow(traits))

#Some of these trait databases are in try. Solution:
#1. For a given Species-trait observation from TRY, 
#if there is a species-trait observation from non-TRY, remove the TRY species-trait observation.
#Go through and do this for all traits from TRY.
to_clean <- c('Ngreen','Pgreen','Nsenes','Psenes','log.LL')
clean <- list()
for(i in 1:length(to_clean)){
  trait_name <- to_clean[i]
  try.spp <- traits[traits$source == 'TRY_database' & !is.na(traits[,trait_name]),]$Species
  ref.spp <- traits[traits$source != 'TRY_database' & !is.na(traits[,trait_name]),]$Species
  to_drop <- try.spp[try.spp %in% ref.spp]
  clean[[i]] <- traits[!(traits$source == 'TRY_database' & traits$Species %in% to_drop),]
}
names(clean) <- to_clean

#2. If we removed a try observation because there was a non-TRY species-trait observation, BUT
#the TRY observation is useful for a another trait, add it back in.
intra_clean <- clean[[1]]
for(i in 2:length(clean)){
  check <- clean[[i]]
  to_add <- check[!(check$unique %in% intra_clean$unique),];nrow(to_add)
  intra_clean <- plyr::rbind.fill(intra_clean,to_add)
}
intra_clean$unique <- NULL
intra_clean <- data.table(intra_clean)

#Get species level means.----
inter_clean <- intra_clean[, lapply(.SD, mean, na.rm = T), by='Species', .SDcols=c('Ngreen','Pgreen','Nsenes','Psenes','Nroots','Proots','log.LL','root_lifespan')]
inter_clean[is.na(inter_clean)] = NA

#save output.----
saveRDS(intra_clean, intra.output.path)
saveRDS(inter_clean, inter.output.path)

#Report.----
n.spp <- nrow(inter_clean)
cat(n.spp,'unique species have some trait data. This is approximately',round(n.spp/400000,2)*100,'% of plant species on Earth.')

#Standardizing species names to The Plant List (TPL) using the Taxonstand package.
#This is done separately here, rather than when generating associated databases, as this takes forever.
#I basically write TPL species lists for mycorrhizal, woodiness and trait databases to files. 
#This function will update those, only checking new names. 
#If this is the first time you have run this, it will take forever.
rm(list=ls())
source('paths.r')
source('functions/cross_ref_tpl.r')
library(data.table)
library(Taxonstand)

#set output path.----
tpl.names.output.path <- tpl_names_lookup.path

#Load data to standardize.----
 traits <- readRDS(merged_intra_traits_names_hand_checked.path)
   myco <- readRDS(merged_myco_traits.path)
   wood <- readRDS(merged_wood_traits.path)
   
#~45k of these already checked from a particular reference. Write them if this is the first time.----
#fred also has tpl assignments...
wood.tpl <- wood[doi == '10.5061/dryad.63q27/2',]$Species
if(!file.exists(tpl.names.output.path)){
  wood.out <- data.frame(wood.tpl,wood.tpl)
  colnames(wood.out) <- c('Species','tpl.Species')
  saveRDS(wood.out,tpl.names.output.path)
}   

#Setup species table, only unique observations.---
all.spp <- data.table(c(traits$Species,myco$Species,wood$Species))
colnames(all.spp) <- c('Species')
all.spp$tpl.Species <- NA
all.spp <- unique(all.spp, by = 'Species')

#Cross reference against species names that have already been standardized to TPL.----
all.spp <- cross_ref_tpl(all.spp, tpl.names.output.path)

#subset assigned and unassigned.----
 assigned <- all.spp[!is.na(tpl.Species),]
to_assign <- all.spp[ is.na(tpl.Species),]

#Assign the unassigned species, merge back together.----
#I could totally hack this to run in parallel on the cluster.
tpl.assignments <- TPL(to_assign$Species)
to_assign$tpl.Species <- paste(tpl.assignments$New.Genus,tpl.assignments$New.Species)

#merge everything back together, update the tpl_names_lookup file.----
all.spp <- rbind(assigned,to_assign)
all.spp[,] <- lapply(all.spp, as.character)
saveRDS(all.spp, tpl.names.output.path)

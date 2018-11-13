#Assign mycorrhizal type, woodiness, worldclim, GBIF climate envelope.
rm(list=ls())
library(data.table)
library(Taxonstand)
library(caper)
library(phytools)
source('paths.r')
source('functions/worldclim2_grab.r')
source('functions/spp_tpl.spp_merge.r')

#Set output.path.
intra.output.path <- intra_specific_analysis_data.path
inter.output.path <- inter_specific_analysis_data.path

#load merged intra-specific data, and databases.----
   d <- data.table(readRDS(merged_intra_traits_names_hand_checked.path))
myco <- data.table(readRDS(merged_myco_traits.path))
myco.gen <- data.table(readRDS(myco_genera_clean.path))
wood <- data.table(readRDS(merged_wood_traits.path))
gbif <- data.table(read.csv(gbif_raw.path))
 tpl <- data.table(readRDS(tpl_names_lookup.path))
 phy <- read.tree(phylogeny.path)

#modify doi names.----
setnames(   d, 'doi','trait_doi')
setnames(myco, 'doi', 'myco_doi')
setnames(wood, 'doi', 'wood_doi')

#assign tpl names.----
   d <- merge(   d, tpl, all.x=T)
myco <- merge(myco, tpl, all.x=T)
wood <- merge(wood, tpl, all.x=T)

#Assign woodiness and mycorrhizal type using species databases.----
#I wrote a special function to do a 4-criteria merge.
#1. Merge on 'Species' in x and y.
#2. For entries still unassigned, merge on 'Species' in x and 'tpl.Species' in y.
#3. For entries still unassigned, merge on 'tpl.Species' in x and 'Species' in y.
#4. For entries still unassigned, merge on 'tpl.Species' in x and y.
d <- spp_tpl.spp_merge(d, wood)
d <- spp_tpl.spp_merge(d, myco)

#Assign mycorrhizal type using genera database.----
#this spot feels a little bit hacky, but works.
  assigned <- d[!is.na(d$MYCO_ASSO),]
unassigned <- d[ is.na(d$MYCO_ASSO),]
unassigned$MYCO_ASSO <- NULL
unassigned$myco_doi <- NULL
unassigned$genus <- gsub( " .*$", "", unassigned$Species)
unassigned$tpl.genus <- gsub( " .*$", "", unassigned$tpl.Species)
myco.gen$tpl.genus <- myco.gen$genus
unassigned <- spp_tpl.spp_merge(unassigned,myco.gen, merge.columns = c('genus','tpl.genus'))
unassigned$genus <- NULL
unassigned$tpl.genus <- NULL
d <- rbind(assigned,unassigned) #re-combine assigned nad unassigned (...which was just assigned)

#Assign gbif climate.----
gbif$X <- NULL
setnames(gbif,c('species','temp','precip'),c('Species','gbif_temp','gbif_precip'))
gbif$tpl.Species <- gbif$Species
d <- spp_tpl.spp_merge(d, gbif)

#Assign plant growth form.-----
#assign angio-gymnosperm status based on phylogeny. 
tree <- phy
clades <- c(
  which(tree$node.label == "Cycadophyta"),
  which(tree$node.label == "Coniferae"),
  which(tree$node.label == "Pinales"),
  which(tree$node.label == "Cupressophyta"),
  which(tree$node.label == "Taxaceae"),
  which(tree$node.label == "Araucariales"),
  which(tree$node.label== "Araucariaceae")
) + length(tree$tip.label)

#mod tip labels
tree$tip.label <- paste0(toupper(substr(tree$tip.label, 1, 1)), substr(tree$tip.label, 2, nchar(tree$tip.label)))
tree$tip.label <- gsub('_',' ',tree$tip.label)
#grab gymno and angio species from phylogeny.
gymno <- unique(unlist(sapply(clades, function(x) getDescendants(tree, x))))
gymno <- tree$tip.label[Filter(function(x) x <= length(tree$tip.label), gymno)]
angio <- setdiff(tree$tip.label, gymno)
gymno <- data.table(gymno);setnames(gymno,'gymno','tpl.Species')
angio <- data.table(angio);setnames(angio,'angio','tpl.Species')
gymno[,pgf:='gymno']
angio[,pgf:='angio']
pgf <- rbind(gymno,angio)
#merge in pgf assignments.
d <- merge(d,pgf, all.x=T)

#grab mat and map based on worldclim.----
clim <- worldclim2_grab(d$latitude, d$longitude)
d <- cbind(d,clim)
intra.out <- d

#get interspecific output.----
q.traits <- c('Ngreen','Pgreen','Nsenes','Psenes','Nroots','Proots','log.LL','root_lifespan','mat','map','gbif_temp','gbif_precip')
d.traits <- c('tpl.Species','Species','MYCO_ASSO','woodiness')
d <- data.table(d)
z <- d[,lapply(.SD, mean, na.rm=T), by = tpl.Species, .SDcols=q.traits]
setkey(d,'tpl.Species')
d <- unique(d, by = 'tpl.Species')
d <- as.data.frame(d)
d <- merge(z,d[,d.traits], by = 'tpl.Species')
#convert NaN values to NA
d[sapply(d,is.na)] = NA 
inter.out <- d

#save output.----
saveRDS(intra.out, intra.output.path)
saveRDS(inter.out, inter.output.path)

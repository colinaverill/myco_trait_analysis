#Assign mycorrhizal type, woodiness, worldclim, GBIF climate envelope.
#clear environment, load packages, functions and file paths.----
rm(list=ls())
library(data.table)
library(Taxonstand)
library(caper)
library(phytools)
source('paths.r')
source('functions/worldclim2_grab.r')
source('functions/spp_tpl.spp_merge.r')
source('functions/wwf_ecoregion_extract.r')

#Set output.paths.----
intra_presub_output.path <- intra_specific_pre.subset_data.path
       intra.output.path <- intra_specific_analysis_data.path
       inter.output.path <- inter_specific_analysis_data.path

#load merged intra-specific data, and databases.----
   d <- data.table(readRDS(merged_intra_traits_names_hand_checked.path))
myco <- data.table(readRDS(merged_myco_traits.path))
myco.gen <- data.table(readRDS(myco_genera_clean.path))
wood <- data.table(readRDS(merged_wood_traits.path))
gbif <- data.table(read.csv(gbif_raw.path))
 tpl <- data.table(readRDS(tpl_names_lookup.path))
 phy <- read.tree(phylogeny_raw.path)
full.tpl <- data.table(readRDS(full_tpl_output.path))
nodDB <- data.table(read.csv(nodDB_raw.path))

#modify doi names.----
setnames(   d, 'doi','trait_doi')
setnames(myco, 'doi', 'myco_doi')
setnames(wood, 'doi', 'wood_doi')

#assign tpl names.----
   d <- merge(   d, tpl, all.x=T)
myco <- merge(myco, tpl, all.x=T)
wood <- merge(wood, tpl, all.x=T)

#Assign plant Family.----
d$tpl.Genus <- gsub( " .*$", "", d$tpl.Species)
full.tpl <- full.tpl[,.(New.Genus,Family)]
setkey(full.tpl,New.Genus)
full.tpl <- unique(full.tpl, by = 'New.Genus')
d <- merge(d,full.tpl[,.(New.Genus,Family)], by.x = 'tpl.Genus', by.y = 'New.Genus', all.x = T)
d <- data.table(d)
d[Family =='', Family := NA]
setnames(d,'Family','tpl.Family')


#Assign woodiness and mycorrhizal type using species databases.----
#I wrote a special function to do a 4-criteria merge.
#1. Merge on 'Species' in x and y.
#2. For entries still unassigned, merge on 'Species' in x and 'tpl.Species' in y.
#3. For entries still unassigned, merge on 'tpl.Species' in x and 'Species' in y.
#4. For entries still unassigned, merge on 'tpl.Species' in x and y.
d <- spp_tpl.spp_merge(d, wood)
d <- spp_tpl.spp_merge(d, myco)

#Assign mycorrhizal type using family and genus-level mycorrhizal knowledge.----
#we know these genera are correct, and we know many of these legacy mycorrhizal databases can have errors.
#Therefore, genus level assignment overrides species match from a reference.
d <- data.table(d)
myco.fam <- myco.gen[Level == 'Family']
myco.gen <- myco.gen[Level == 'Genus' ]

#Genus level assignment.
to_assign <- d[  tpl.Genus %in% myco.gen$ID ]
assigned  <- d[!(tpl.Genus %in% myco.gen$ID)]
to_assign$myco_doi <- NULL
to_assign$MYCO_ASSO <- NULL
to_assign <- merge(to_assign, myco.gen[,.(ID,MYCO_ASSO,myco_doi)], by.x='tpl.Genus',by.y ='ID', all.x = T)
d <- rbind(assigned,to_assign)

#Family level assignment.
to_assign <- d[  tpl.Family %in% myco.fam$ID ]
assigned  <- d[!(tpl.Family %in% myco.fam$ID)]
to_assign$myco_doi <- NULL
to_assign$MYCO_ASSO <- NULL
to_assign <- merge(to_assign, myco.fam[,.(ID,MYCO_ASSO,myco_doi)], by.x='tpl.Family',by.y ='ID', all.x = T)
d <- rbind(assigned,to_assign)

#Assign Nfix using NodDB.----
yes.nfix  <- c('Rhizobia','likely_Rhizobia','Frankia','Nostocaceae','likely_present','Present')
yes.nfix2 <- c('Rhizobia','Frankia','Nostocaceae','Present')
nodDB[Consensus.estimate %in% yes.nfix , nfix  := 1]
nodDB[Consensus.estimate %in% yes.nfix2, nfix2 := 1]
nodDB <- nodDB[nfix == 1,.(genus,nfix,nfix2)]
d$tpl.Genus <- gsub( " .*$", "", d$tpl.Species)
d$nfix  <- ifelse(d$tpl.Genus %in% nodDB$genus, 1, 0)
d$nfix2 <- ifelse(d$tpl.Genus %in% nodDB[nfix2 == 1,]$genus, 1, 0)

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

#assign biome.----
biome <- wwf_ecoregion_extract(d$latitude, d$longitude)
biome <- data.table(biome)
d <- cbind(d, biome[,.(ECO_NAME,biome_name,biome_name2)])

#grab mat and map based on worldclim.----
clim <- worldclim2_grab(d$latitude, d$longitude)
d <- cbind(d,clim)

#Subset to match study inclusion criteria.----
pre_subset_intra_out <- d
d <- data.table(d)
d <- d[MYCO_ASSO %in% c('AM','ECM')] #Only AM and ECM species.
d <- d[!is.na(pgf),]                 #Analysis requires growth-form assignment (angio/gymno)
d <- d[woodiness == 'W',]            #Only woody species (herbaceous are all AM in this data set).
#set some reasonable constraint on tissue element concentrations.
#These constraints only affect green leaf elements, and remove less than 0.5% of observations.
nrow(d[!is.na(Ngreen) & Ngreen < 200])/nrow(d[!is.na(Ngreen)])
nrow(d[!is.na(Pgreen) & Pgreen <  20])/nrow(d[!is.na(Pgreen)])
nrow(d[!is.na(Nsenes) & Nsenes < 200])/nrow(d[!is.na(Nsenes)])
nrow(d[!is.na(Psenes) & Psenes < 200])/nrow(d[!is.na(Psenes)])
nrow(d[!is.na(Nroots) & Nroots < 200])/nrow(d[!is.na(Nroots)])
nrow(d[!is.na(Proots) & Proots < 200])/nrow(d[!is.na(Proots)])
d <- d[Ngreen < 200 & Ngreen > 0 | is.na(Ngreen),]
d <- d[Nsenes < 200 & Nsenes > 0  | is.na(Nsenes),]
d <- d[Nroots < 200 & Nroots > 0  | is.na(Nroots),]
d <- d[Pgreen <  20 & Pgreen > 0  | is.na(Pgreen),]
d <- d[Psenes <  20 & Psenes > 0  | is.na(Psenes),]
d <- d[Proots <  20 & Proots > 0  | is.na(Proots),]
#only include species in phylogeny.
d$Species <- d$tpl.Species
tree$node.label <- NULL
d <- d[tpl.Species %in% tree$tip.label,]
intra.out <- d

#get interspecific output.----
q.traits <- c('Ngreen','Pgreen','Nsenes','Psenes','Nroots','Proots','log.LL','root_lifespan','mat','map','gbif_temp','gbif_precip')
d.traits <- c('tpl.Species','Species','MYCO_ASSO','woodiness','pgf','nfix','nfix2')
#grab most frequent biome, accounts for "ties".
#biome_out <- table(d[,.(tpl.Species,biome_name)])
library(dplyr)
biome_out1 <-  as.data.frame(
    d %>%
    dplyr::count(tpl.Species, biome_name) %>%
    dplyr::group_by(tpl.Species) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::mutate(r = row_number()) %>%
    tidyr::spread(r, biome_name) %>%
    dplyr::select(-n))
colnames(biome_out1) <- c('tpl.Species','biome1')
biome_out1[] <- lapply(biome_out1, as.character)
biome_out2 <-  as.data.frame(
  d %>%
    dplyr::count(tpl.Species, biome_name2) %>%
    dplyr::group_by(tpl.Species) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::mutate(r = row_number()) %>%
    tidyr::spread(r, biome_name2) %>%
    dplyr::select(-n))
colnames(biome_out2) <- c('tpl.Species','biome2')
biome_out2[] <- lapply(biome_out2, as.character)
detach('package:dplyr')
#There are only 2 species with equally frequent biome categorizations, and they don't really matter.
#Just set everything to the first biome column.
biome_out1 <- biome_out1[,c('tpl.Species','biome1')]
biome_out2 <- biome_out2[,c('tpl.Species','biome2')]
biome_out <- merge(biome_out1, biome_out2)

#get quantitative trait means.
d <- data.table(d)
z <- d[,lapply(.SD, mean, na.rm=T), by = tpl.Species, .SDcols=q.traits]
setkey(d,'tpl.Species')
#merge in discrete traits.
d <- unique(d, by = 'tpl.Species')
d <- as.data.frame(d)
d <- merge(z,d[,d.traits], by = 'tpl.Species')
d <- merge(d, biome_out, all.x = T)

#Get mat.c and map.c, where these are first taken from gbif, and then from itra-specific observations.
d[,mat.c := gbif_temp]
d[is.na(mat.c), mat.c := mat]
d[,map.c := gbif_precip]
d[is.na(map.c), map.c := map]

#convert NaN values to NA
d <- as.data.frame(d)
d[sapply(d,is.na)] = NA 
inter.out <- d

#save output.----
saveRDS(pre_subset_intra_out, intra_presub_output.path)
saveRDS(intra.out, intra.output.path)
saveRDS(inter.out, inter.output.path)

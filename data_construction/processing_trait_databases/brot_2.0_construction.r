#Processing BROT2.0 database.
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.----
output.path <- brot_intra.path

#load data.----
d <- data.table(read.csv(brot_trait_raw.path))
spp <- data.table(read.csv(brot_tax_raw.path))
#ref <- data.table(read.csv('/fs/data3/caverill/myc_traits/BROT_2.0_database/BROT2_sou.csv'))
doi <- "10.6084/m9.figshare.c.3843841"

#grab leaf lifespan observations.----
ll <- d[Trait == 'LeafLifespan']

#merge in actualy species names.----
spp$g.spp <- paste(spp$Genus, spp$Species, sep= ' ')
ll <- merge(ll, spp[,.(ID, g.spp)], by.x = 'TaxonID', by.y = 'ID', all.x = T)
setnames(ll,'g.spp','Species')

#convert to log.LL and rename.----
ll$Data <- as.numeric(as.character(ll$Data))
ll$Data <- log(ll$Data, base = 10)
setnames(ll,'Data','log.LL')

#grab columns of interest.----
to_return <- ll[,.(Species,log.LL,Lat,Long)]
setnames(to_return,c('Lat','Long'),c('latitude','longitude'))
to_return$doi <- doi

#fix a single species name.----
to_return$Species <- gsub('Quercus x cerrioides','Quercus cerrioides',to_return$Species)

#Save output.----
saveRDS(to_return, output.path)

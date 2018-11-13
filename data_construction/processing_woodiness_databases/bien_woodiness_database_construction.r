#BIEN species woodiness harmonization.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- bien_wood_clean.path

#load data.----
library(BIEN)
spp <- BIEN_list_all()
spp <- as.vector(spp[,1])
of_interest <- c('whole plant woodiness')
n.brk <- 60
brk <- round(length(spp)  / n.brk)
trait.out <- list()
for(i in 1:n.brk){
  start <- (brk*i - brk) + 1
  end <- brk*i
  if(i == n.brk){end = length(spp)}
  traits <- BIEN_trait_species(spp[start:end])
  traits <- traits[traits$trait_name %in% of_interest,]
  trait.out[[i]] <- traits
}
traits <- do.call('rbind',trait.out)
traits <- data.table(traits)


#Grab columns of interest, assign doi.----
b.woody  <- traits[trait_name %in% c('whole plant woodiness')]
b.woody <- b.woody[!duplicated(scrubbed_species_binomial),]
bien <- b.woody[,.(scrubbed_species_binomial,trait_value)]
colnames(bien) <- c('Species','woody')
bien[,doi:='10.1111/2041-210X.12861']

#Harmonize woodiness assignments.----
bien[woody == 'woody'     ,woodiness := 'W']
bien[woody == 'herbaceous',woodiness := 'H']
bien <- bien[!is.na(woodiness),.(Species,woodiness,doi)]

#Save output.----
saveRDS(bien,output.path)

#Extracting BIEN database data.
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)
library(BIEN)

#set output paths.----
intra.output.path <- bien_intra.path
inter.output.path <- bien_inter.path

#load BIEN library and data.----
#get all bien species - 257,876. Wow.
spp <- BIEN_list_all()
#remove a few problem species.
spp <- spp[!(spp$species %in% c("Ewartia 'sinclairii intergen hybrid sinclairii x Helichrysum bellidioides","Eugenia corusca")),]

#Query BIEN database (could be sped up via parallelization foreach loop).----
#we need to query in batches, only retain the traits we want. Too much data otherwise.
of_interest <- c('leaf nitrogen content per leaf dry mass','leaf phosphorus content per leaf dry mass','whole plant woodiness')
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

#go ahead as usual.
#build intra-specific table.----
b.Ngreen <- traits[trait_name %in% c('leaf nitrogen content per leaf dry mass')]
b.Pgreen <- traits[trait_name %in% c('leaf phosphorus content per leaf dry mass')]
b.woody  <- traits[trait_name %in% c('whole plant woodiness')]
b.Ngreen[,trait_value := as.numeric(trait_value)]
b.Pgreen[,trait_value := as.numeric(trait_value)]
setnames(b.Ngreen, 'trait_value','Ngreen')
setnames(b.Pgreen, 'trait_value','Pgreen')
setnames(b.woody , 'trait_value','woody' )
bien.intra.out <- merge(b.Ngreen[,.(scrubbed_species_binomial,Ngreen,latitude,longitude,id)],
                        b.Pgreen[,.(scrubbed_species_binomial,Pgreen,latitude,longitude,id)], 
                        by = c('scrubbed_species_binomial','latitude','longitude','id'), all = T)
bien.intra.out$id <- NULL
setnames(bien.intra.out,'scrubbed_species_binomial','Species')
bien.intra.out$source <- rep('bien',nrow(bien.intra.out))
bien.intra.out$doi    <- rep('10.1111/2041-210X.12861',nrow(bien.intra.out))


#get inter-specific output.----
bien.inter.out <- bien.intra.out[, lapply(.SD, mean, na.rm = T), by='Species', 
                                 .SDcols=c('Ngreen','Pgreen')]
bien.inter.out$source <- rep('bien'                   ,nrow(bien.inter.out))
bien.inter.out$doi    <- rep('10.1111/2041-210X.12861',nrow(bien.inter.out))
bien.inter.out[is.na(bien.inter.out)] = NA

#save output.----
saveRDS(bien.intra.out,intra.output.path)
saveRDS(bien.inter.out,inter.output.path)

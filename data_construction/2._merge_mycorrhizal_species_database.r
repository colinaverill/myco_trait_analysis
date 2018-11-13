#Building a dataframe of plant species, mycorrhizal association, and associated doi.
rm(list=ls())
source('paths.r')
library(data.table)

#output path
output.path <- merged_myco_traits.path

#load up some mycorrhizal data sets.
fred <- readRDS(fred_myco_clean.path)
fia  <- readRDS(fia_myco_clean.path)
ted  <- read.csv(tedersoo_myco_genera.path)
e093 <- readRDS(e093_myco_clean.path)
mflo <- readRDS(mycoflor_myco_clean.path)
opik <- readRDS(opik_myco_clean.path)
harl <- readRDS(harley_harley_myco_clean.path)
avki <- readRDS(averill_kivlin_myco_clean.path)

#Merge these together.
out <- fred
out <- rbind(out, fia[!(Species %in% out$Species)])
out <- rbind(out,e093[!(Species %in% out$Species)])
out <- rbind(out,mflo[!(Species %in% out$Species)])
out <- rbind(out,opik[!(Species %in% out$Species)])
out <- rbind(out,harl[!(Species %in% out$Species)])
out <- rbind(out,avki[!(Species %in% out$Species)])

#Make sure everything is character.
out[,] <- lapply(out, as.character)

#save output
saveRDS(out,output.path)

#Report.----
n.spp <- nrow(out)
cat(n.spp,'unique species have mycorrhizal assignments. This is approximately',round(n.spp/400000,3)*100,'% of plant species on Earth.')

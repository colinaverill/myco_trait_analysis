#Merging plant woodiness databases.
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.----
output.path <- merged_wood_traits.path

#load harmonized plant woodiness datasets.----
zann <- readRDS(zann_wood_clean.path)
fred <- readRDS(fred_wood_clean.path)
ornl <- readRDS(ornl_wood_clean.path)
glop <- readRDS(glop_wood_clean.path)
cptd <- readRDS(cptd_wood_clean.path)
bien <- readRDS(bien_wood_clean.path)

#Begin merge.----
out <- zann
out <- rbind(out,fred[!(Species %in% out$Species),])
out <- rbind(out,ornl[!(Species %in% out$Species),])
out <- rbind(out,glop[!(Species %in% out$Species),])
out <- rbind(out,cptd[!(Species %in% out$Species),])
out <- rbind(out,bien[!(Species %in% out$Species),])

#Deal with some non-ascii characters.----
#probably a way to recover these 17 species.
grepNonASCII <- function(x) {
  asc <- iconv(x, "latin1", "ASCII")
  ind <- is.na(asc) | asc != x
  which(ind)
}
out <- out[!(grepNonASCII(Species)),]


#Make sure everything is character.
out[,] <- lapply(out, as.character)

#Save output.----
saveRDS(out,output.path)

#Report.----
n.spp <- nrow(out)
cat(n.spp,'unique species have woodiness assignments. This is approximately',round(n.spp/400000,2)*100,'% of plant species on Earth.')

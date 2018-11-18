#genera level mycorrhizal database construction.
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.
output.path <- myco_genera_clean.path

#load genus level mycorrhizal databases.----
ted <- read.csv(tedersoo_myco_genera.path)
ted$myco_doi <- '10.1007/978-3-319-56363-3_19'
poa <- readRDS(poaceae_genera_raw.path)
poa$MYCO_ASSO <- rep('AM',nrow(poa))
poa$myco_doi <- '10.1111/jse.12150'
poa <- data.table(poa)
setkey(poa, genus)
poa <- unique(poa, by = 'genus')
#A few AM genera from Brudrett and Tedersoo 2018 New Phyt. doi: 10.1111/nph.15440 
gen <- c('Fraxinus','Ulmus','Acer')
myc <- rep('AM', length(gen))
myc_doi <- rep('10.1111/nph.15440',length(gen))
br.t <- data.frame(gen,myc,myc_doi)
colnames(br.t) <- colnames(ted)

#generate merged output and save.----
genera_merge <- rbind(ted,poa, br.t)
saveRDS(genera_merge,output.path)

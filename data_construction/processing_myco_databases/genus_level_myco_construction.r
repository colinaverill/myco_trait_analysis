#genera level mycorrhizal database construction.
rm(list=ls())
library(data.table)
source('paths.r')

#set output path.
output.path <- myco_genera_clean.path

#load genus and family level mycorrhizal databases.----
ted <- read.csv(tedersoo_myco_genera.path)
ted$myco_doi <- '10.1007/978-3-319-56363-3_19'
b_t <- data.table(read.csv(brundrett_tedersoo_families.path))
b_t$myco_doi <- '10.1111/nph.15440'

#mod b_t designations.----
b_t$Mycorrhiza <- as.character(b_t$Mycorrhiza)
b_t[Mycorrhiza == 'EM', Mycorrhiza := 'ECM']
b_t <- b_t[!(Family == 'Ericaceae')]

#A few AM genera from Brudrett and Tedersoo 2018 New Phyt. doi: 10.1111/nph.15440.----
gen <- c('Fraxinus','Ulmus','Acer')
myc <- rep('AM', length(gen))
myc_doi <- rep('10.1111/nph.15440',length(gen))
br.t <- data.frame(gen,myc,myc_doi)
colnames(br.t) <- colnames(ted)

#Specify phylogenetic level.----
b_t$level <- 'Family'
ted$level <- 'Genus'
br.t$level <- 'Genus'

#change column names.----
colnames(ted) <- c('ID','MYCO_ASSO','myco_doi','Level')
colnames(b_t) <- colnames(ted)
colnames(br.t)<- colnames(ted)

#generate merged output and save.----
all_merge <- rbind(ted, b_t, br.t)
saveRDS(all_merge,output.path)

#Processing woodiness data from FRED 2.0
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- fred_wood_clean.path

#load data.----
headers <- read.csv(fred_2.0_raw.path, header = F, nrows = 1)
headers <- data.frame(lapply(headers, as.character), stringsAsFactors=FALSE)
headers <- as.character(headers[1,])
headers <- gsub(' ','.',headers) #why are there spaces in the column names...
fred <- read.csv(fred_2.0_raw.path, header = F,  skip = 7)
colnames(fred) <- headers

#Grab columns of interst
of_interest <- c('Accepted.genus_TPL','Accepted.species_TPL','Data.source_DOI',
                 'Belowground.part','Root.vitality_Roots.living.or.dead',
                 'Plant.growth.form')
fred <- fred[,colnames(fred) %in% of_interest]
fred <- data.table(fred)

#Only take fine roots and living roots.----
#living roots.
keepers <- unique(fred$Root.vitality_Roots.living.or.dead)
keepers <- keepers[!(keepers %in% c('dead'))]
fred <- fred[fred$Root.vitality_Roots.living.or.dead %in% keepers]
#fine roots only (<2mm).
keepers <- c('FR')
nrow(fred[fred$Belowground.part %in% keepers,])

#trim white space, assign species.----
fred[,  Accepted.genus_TPL := trimws(fred$Accepted.genus_TPL  )]
fred[,Accepted.species_TPL := trimws(fred$Accepted.species_TPL)]
fred[,Species := paste(Accepted.genus_TPL,Accepted.species_TPL,sep = ' ')]
#remove entries with no species level observation.
fred <- fred[!(Species %in% c('',' ')),]

#add source information.----
fred[,source := 'fred']
#fred[,doi := '10.1111/nph.14486'] #use original study doi if available.

#grab columns of interest.----
fred <- fred[!is.na(Plant.growth.form),.(Species,Plant.growth.form,Data.source_DOI)]
setnames(fred,'Data.source_DOI','doi')

#Harmonize woodiness assignments.----
fred[,woody := Plant.growth.form %in% c('Shrub','shrub','shrub/tree','subshrub/liana','tree','liana') + 0L]
fred[Plant.growth.form %in% c('',' ', 'F00032','Plant growth form'), woody := NA]
#Make sure everything is unique
setkey(fred,'Species')
fred <- unique(fred, by = 'Species')
fred[woody ==1, woodiness := 'W']
fred[woody ==0, woodiness := 'H']
fred <- fred[!is.na(woodiness)]
fred <- fred[,.(Species,woodiness,doi)]
#fill out empty doi's with general fred doi.
fred[doi == '', doi:= '10.1111/nph.14486']

#Save output.----
saveRDS(fred,output.path)

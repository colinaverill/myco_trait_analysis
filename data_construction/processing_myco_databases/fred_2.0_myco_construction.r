#FRED mycorrhizal data construction.
rm(list=ls())
source('paths.r')
library(data.table)

#set output path.----
output.path <- fred_myco_clean.path

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
                 'Root.N.content','Root.P.content','Plant.growth.form',
                 'Mycorrhiza_Type','Root.mean.lifespan_d',
                 'Latitude_main','Longitude_Main')
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

###FRED - 494 unique mycorrhizal assignments.----
fred <- fred[,.(Species,Mycorrhiza_Type,Data.source_DOI)]
setnames(fred,old=c('Mycorrhiza_Type','Data.source_DOI'), new=c('MYCO_ASSO','doi'))

#only take species that have mycorrhizal assignments.----
fred$MYCO_ASSO <- as.character(fred$MYCO_ASSO)
fred <- fred[!(MYCO_ASSO %in% c('','EeM','F00645','Mycorrhizal type'))]

#rename mycorrhizal assignments to harmonize.----
fred[MYCO_ASSO == 'AM + EM', MYCO_ASSO := 'AM_ECM']
fred[MYCO_ASSO == 'EM'     , MYCO_ASSO := 'ECM'   ]
fred[MYCO_ASSO == 'ErM'    , MYCO_ASSO := 'ERM'   ]

#Grab unique species.----
setkey(fred, Species)
fred <- unique(fred, by = "Species")
#fill out empty doi's with general fred doi.
fred[doi == '', doi:= '10.1111/nph.14486']

#Save output.----
saveRDS(fred,output.path)

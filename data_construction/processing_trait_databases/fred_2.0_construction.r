#organizing FRED 2.0 data. doi: 10.1111/nph.14486
rm(list=ls())
source('paths.r')
library(data.table)
library(caper)
library(gsheet)
library(stringi)

#set output paths.----
intra.output.path <- fred_2.0_intra.path
inter.output.path <- fred_2.0_inter.path

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
#fill out empty doi's with general fred doi.
fred[doi == '', doi:= '10.1111/nph.14486']

#subset to columns of interest and rename as appropriate.----
setnames(fred,
         c('Data.source_DOI','Root.N.content','Root.P.content','Root.mean.lifespan_d','Latitude_main','Longitude_Main'),
         c('doi','Nroots','Proots','root_lifespan','latitude','longitude'))
#Further subsetting.
fred <- fred[,.(Species, Nroots, Proots, root_lifespan, latitude, longitude, source, doi)]
fred <- fred[!(is.na(Nroots) & is.na(Proots) & is.na(root_lifespan)),]


#save intra-specific product.----
fred.intra.out <- fred

#Get species level means.----
z <- fred[, lapply(.SD, mean, na.rm = T), by=Species, .SDcols=c('Nroots','Proots','root_lifespan')]
setkey(fred,'Species')
fred <- unique(fred, by = 'Species')
fred <- merge(z,fred[,.(Species,source)], by = 'Species')
#convert NaN values to NA
fred[sapply(fred,is.na)] = NA 
fred.inter.out <- fred[,.(Species,Nroots,Proots,root_lifespan,source)]
fred.inter.out$doi <- '10.1111/nph.14486'

#Save output.
saveRDS(fred.intra.out, intra.output.path)
saveRDS(fred.inter.out, inter.output.path)

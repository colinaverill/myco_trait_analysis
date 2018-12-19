# Building climate data (somewhat quickly)
# Will Pearse - 2018-05016
rm(list=ls())
library(data.table)
library(raster)
library(RNetCDF)
source('paths.r')

#load your points
d <- data.table(readRDS(merged_intra_traits_names_hand_checked.path))

#set output directory.
output.dir <- '/fs/data3/caverill/GBIF/'

#progress bar function.----
prog.bar <- function(x, y){
  if(y < 100){
    cat(".")} else {
      z <- Filter(function(z) z>=0, seq(1,y,length.out=100)-x)
      if(length(z) > 0)
        tryCatch(if(z[1] < 1) if((length(z) %% 10)==0) cat("|") else cat("."), error=function(z) cat("."))
    }
}    


#########################
# GBIF prep #############
#########################
# Load data - human observations only
data <- fread("/fs/data3/caverill/GBIF/gbif_cut.txt")
data$type <- "observation"

# Subset by year
data <- data[year > 1900 & year <= 2016,]

# Sort by year (to help for later)
data <- data[order(year),]

# Load synonymy list and subset by that
syns <- read.csv("tpl_syn.txt", as.is=TRUE)
syns$syn.names <- sapply(strsplit(syns$synonym, " "), function(x) paste(x[1:2], collapse=" "))
t <- data.frame(input=unique(syns$input), synonym="accpted.for.safety", status="accepted", syn.names=unique(syns$input))
syns <- rbind(syns, t)

data <- data[species %in% unique(syns$syn.names),]

#########################
# Climate data ##########
#########################
# Function to average climate data by year
year.avg <- function(x, name, years=1901:2016){
  output <- array(dim=c(dim(x[[name]])[1:2], length(years)))
  for(i in seq_len(dim(output)[3])){
    prog.bar(i, dim(output)[3])
    curr <- seq(12*(i-1)+1, by=1, length.out=12)
    output[,,i] <- apply(x[[name]][,,curr], 1:2, mean, na.rm=TRUE)
  }
  return(output)
}
# Function to convert climate data to raster.
create.stack <- function(x, long=seq(-179.75,179.75,by=0.5), lat=seq(-89.75,89.75,by=0.5)){
  output <- raster(t(x[,,1][,ncol(x):1]), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat))
  base <- raster(xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat))
  for(i in seq(2, dim(x)[3]))
    output <- stack(output, raster(t(x[,,i][,ncol(x):1]), xmn=min(long), xmx=max(long), ymn=min(lat), ymx=max(lat)))
  return(output)
}

  temp <- create.stack(year.avg(read.nc(open.nc("/fs/data3/caverill/GBIF/cru_ts4.02.2001.2010.tmp.dat.nc")), "tmp"))
precip <- create.stack(year.avg(read.nc(open.nc("/fs/data3/caverill/GBIF/cru_ts4.02.2001.2010.pre.dat.nc")), "pre"))

########################
# Calculate climate ####
########################
data$temp <- -999; data$precip <- -999
years <- seq(1901,2016)

for(i in seq_along(years)){
  limits <- range(which(data$year==years[i]), na.rm=TRUE)
  chunks <- seq(limits[1], limits[2], by=20000)
  if(chunks[length(chunks)] != limits[2])
    chunks <- c(chunks, limits[2])
  for(j in seq(1, length(chunks)-1)){
    data[seq(chunks[j],chunks[j+1]),]$temp <- extract(temp[[years[i]-1900]], data[seq(chunks[j],chunks[j+1]),.(decimallongitude,decimallatitude)])
    data[seq(chunks[j],chunks[j+1]),]$precip <- extract(precip[[years[i]-1900]], data[seq(chunks[j],chunks[j+1]),.(decimallongitude,decimallatitude)])
  }
}
fwrite(data, paste0(output.dir,"plant_climate.csv"))

# Average by species/synonyms
true.species <- unique(syns$input)
temp <- precip <- numeric(length(true.species))
for(i in seq_along(true.species)){
  t.data <- data[species %in% syns$syn.names[syns$input == true.species[i]],]
  temp[i] <- mean(t.data$temp, na.rm=TRUE)
  precip[i] <- mean(t.data$precip, na.rm=TRUE)
}

species <- data.frame(species, temp, precip)
write.csv(species, paste0(output.dir,"species_means.csv"))

fwrite(data[, mean(temp,na.rm=TRUE), by=scientificname], paste0(output.dir,"plant_mean_temp.csv"))
fwrite(data[, mean(precip,na.rm=TRUE), by=scientificname], paste0(output.dir,"plant_mean_precip.csv"))

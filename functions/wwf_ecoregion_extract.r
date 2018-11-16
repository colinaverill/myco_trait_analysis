#' wwf_ecoregion_extract.r
#'extract ecoregions from WWF biomes, found here: 
#'https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
#'shape_path variable must point to unzipped directory downloaded from above link.
#'depends on: sp, rgdal, plyr, and associated dependencies.
#' @param latitude   #vector of latitude
#' @param longitude  #vector of longitide
#' @param shape_path #path to unzipped directory from wwf, link in above description.
#'
#' @return           #returns wwf ecoregions and biomes, with your longitude,latitude.
#' @export
#'
#' @examples
#'  latitude <- c( 44.2833, 44.5167,  35.28223324,  35.28223324, NA, 47.18333333)
#' longitude <- c(-71.2667,   -72.8, -111.6241957, -111.6241957, NA, 128.8833333)
#' wwf_ecoregion_extract(latitude, longitude)
wwf_ecoregion_extract <- function(latitude, longitude, 
                              shape_path = '/fs/data3/caverill/wwf_ecoregions/official/'){
  #make sure you have the packages.
  check <- installed.packages()
  check <- check[,1]
  if('sp'    %in% check == F){stop('Install package "sp"'   )}
  if('rgdal' %in% check == F){stop('Install package "rgdal"')}
  if('plyr'  %in% check == F){stop('Install package "plyr"' )}
  
  #hand copying biome key from: http://omap.africanmarineatlas.org/BIOSPHERE/data/note_areas_sp/Ecoregions_Ecosystems/WWF_Ecoregions/WWFecoregions.htm
  biome_num  <- c(1:14)
  biome_name <- c('Tropical and Subtropical Moist Broadleaf Forests','Tropical and Subtropical Dry Broadleaf Forests',
                  'Tropical and Subtropical Coniferous Forests','Temperate Broadleaf and Mixed Forests',
                  'Temperate Conifer Forests','Boreal Forests and Taiga','Tropical and Subtropical Grasslands Savannas and Shrublands',
                  'Temperate Grasslands Savannas and Shrublands','Flooded Grasslands and Savannas','Montane Grasslands and Shrublands',
                  'Tundra','Mediterranean Forests Woodlands and Scrub','Deserts and Xeric Shrublands','Mangroves')
  biome_name <- gsub(' ','_',biome_name)
  biome_key <- data.frame(biome_num,biome_name)
  
  #gotta deal with NAs in points because these spatial frickin geniuses did not.
  to_assign <- data.frame(longitude, latitude)
  to_assign$unq <- seq(1:nrow(to_assign))        #unique key to re-order post extract non-NAs.
  yup  <- to_assign[ complete.cases(to_assign),] #non-NAs.
  nope <- to_assign[!complete.cases(to_assign),] #NAs.
  
  #extract the damn layer.
  points <- cbind(yup$longitude, yup$latitude)   #points object.
   layer <- rgdal::readOGR(shape_path, 'wwf_terr_ecos') #load layer.
     pts <- sp::SpatialPoints(points, proj4string = sp::CRS(sp::proj4string(layer))) #project points.
  output <- sp::over(pts, layer)                 #extract the layer.
  output <- data.frame(cbind(yup, output))       #add in lat/long and unq value to sort.
  output <- plyr::rbind.fill(output, nope)       #add back in the NAs.
  output <- output[order(output$unq),]           #reorder the damn thing.
  output$unq <- NULL
  
  #merge in the real biome names.
  output <- merge(output, biome_key,
                  by.x='BIOME',
                  by.y='biome_num',
                  all.x = T)
  return(output)                                 #send it.
}

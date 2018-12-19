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
  biome_name <- c('Tropical_and_Subtropical_Moist_Broadleaf_Forests','Tropical_and_Subtropical_Dry_Broadleaf_Forests',
                  'Tropical_and_Subtropical_Coniferous_Forests','Temperate_Broadleaf_and_Mixed_Forests',
                  'Temperate_Conifer_Forests','Boreal_Forests_and_Taiga','Tropical_and_Subtropical_Grasslands_Savannas_and_Shrublands',
                  'Temperate_Grasslands_Savannas_and_Shrublands','Flooded_Grasslands_and_Savannas','Montane_Grasslands_and_Shrublands',
                  'Tundra','Mediterranean_Forests_Woodlands_and_Scrub','Deserts_and_Xeric_Shrublands','Mangroves')
  biome_name2 <- c('Tropical_forest','Tropical_forest','Tropical_forest','Temperate_forest','Temperate_forest',
                   'Boreal_forest','Tropical_grass_savannah','Temperate_grass_savannah','Flooded_grass_savannah',
                   'Montane_grass_savannah','Tundra','Mediterranean_woodland','Desert','Mangrove')
  biome_name3 <- c('Forest','Forest','Forest','Forest','Forest','Forest','Savannah','Savannah','Wetland','Savannah',
                   'Tundra_Desert','Savannah','Tundra_Desert','Wetland')
       forest <- ifelse(biome_name3 == 'Forest',1,0)
  biome_key <- data.frame(biome_num,biome_name,biome_name2,forest)
  
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
  
  #merge in the real biome names.
  output <- merge(output, biome_key,
                  by.x='BIOME',
                  by.y='biome_num',
                  all.x = T)
  output <- output[base::order(output$unq),]     #reorder the damn thing.
  #output$unq <- NULL
  return(output)                                 #send it.
}

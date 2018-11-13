#' Extract worldclim2 MAT, MAP, MAT_CV, MAP_CV, at 30s resolution for a given latitude/longtiude.
#' 
#'
#' @param latitude         vector of latitude in decimal degrees.
#' @param longitude        vector of longitude in decimal degrees.
#' @param worldclim2_foder path to directory of worldclim2 rasters.
#'
#' @return           returns a datafrme with mat, map, mat_CV, map_CV and MDR
#' @export
#'
#' @examples
#' points <- structure(c(-148.274862, -148.274862, -148.2747566, -148.2747566, 
#'                      -148.2746513, -148.2746513, -148.2744406, -148.2744406, -148.2740191, 
#'                      -148.2740191, 64.766184, 64.766184, 64.766184, 64.766184, 64.766184, 
#'                      64.766184, 64.766184, 64.766184, 64.766184, 64.766184), .Dim = c(10L, 
#'                                                                                      2L))
#' test.out <- worldclim2_grab(points[,2], points[,1])
worldclim2_grab <- function(latitude, longitude, worldclim2_folder = '/fs/data3/caverill/WorldClim2/'){
  
  #make points an object
  points <- cbind(longitude, latitude)
 
  #load mean annual temperature and precipitation rasters from worldclim2
  prec    <- raster::raster(paste0(worldclim2_folder,'wc2.0_bio_30s_12.tif'))
  temp    <- raster::raster(paste0(worldclim2_folder,'wc2.0_bio_30s_01.tif'))
  temp_CV <- raster::raster(paste0(worldclim2_folder,'wc2.0_bio_30s_04.tif'))
  prec_CV <- raster::raster(paste0(worldclim2_folder,'wc2.0_bio_30s_15.tif'))
  mdr     <- raster::raster(paste0(worldclim2_folder,'wc2.0_bio_30s_02.tif'))
  

  #extract worldclim2 predicted climate data.
     prec.obs <- raster::extract(prec, points)
     temp.obs <- raster::extract(temp, points)
  prec_CV.obs <- raster::extract(prec_CV, points)
  temp_CV.obs <- raster::extract(temp_CV, points)
      mdr.obs <- raster::extract(    mdr, points)
  

  #wrap up output and return.
  to_return <- data.frame(cbind(prec.obs,temp.obs,prec_CV.obs,temp_CV.obs,mdr.obs))
  colnames(to_return) <- c('map','mat','map_CV','mat_CV','mdr')
  return(to_return)
  
} #end function.

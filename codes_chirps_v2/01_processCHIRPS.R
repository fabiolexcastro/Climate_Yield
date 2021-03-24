

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, foreach, doSNOW)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
extract_mask <- function(year){
  
  print(year)
  fle <- grep(year, fles, value = TRUE)
  
  print('To extract by mask')
  cl <- makeCluster(12)
  registerDoSNOW(cl)
  
  foreach(i = 1:length(fle), .verbose = TRUE) %dopar% {
    
    require(pacman)
    pacman::p_load(raster, rgdal, rgeos)
    
    shpf <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/base/MGN_DPTO_POLITICO.shp')
    rst <- raster(fle[i])
    rst <- raster::crop(rst, shpf)
    rst <- raster::mask(rst, shpf)
    rst <- rst * 1
    out <- './workspace/CHIRPS_v2/raster/chirps/colombia/'
    writeRaster(rst, filename = paste0(out, basename(fle[i])), overwrite = TRUE)
    
  }
  
  stopCluster(cl)
  print(paste0('Done ', year))  
  
}


# Load data ---------------------------------------------------------------
root <- '//catalogue/BaseLineDataCluster01/observed/gridded_products/chirps/daily'
fles <- list.files(root, full.names = TRUE, pattern = '.tif$')


# Get the years available--------------------------------------------------
years <- unique(str_sub(basename(fles), start = 13, end = 16))

# To extract by mask for only Colombia ------------------------------------
for(j in 1:length(years)){
  
  print(years[j])
  extract_mask(year = years[j])
  print(' Done')
  
}




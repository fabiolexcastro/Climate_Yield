
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
root <- '//dapadfs/data_cluster_4/observed/gridded_products/chirps/daily/32bits'
fles <- list.files(root, full.names = T, pattern = '.tif$')
shpf <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/base/MGN_DPTO_POLITICO.shp')

# Function
extract_mask <- function(year){
  
  # year <- 1981
  
  print(year)
  fle <- grep(year, fles, value = TRUE)
  rst <- list()
  
  for(i in 1:length(fle)){
    
    print(fle[i])
    rst[[i]] <- raster(fle[i])
    rst[[i]] <- raster::crop(rst[[i]], shpf)
    rst[[i]] <- raster::mask(rst[[i]], shpf)
    rst[[i]] <- rst[[i]] * 1
    writeRaster(rst[[i]], filename = paste0('./raster/chirps/colombia/', basename(fle[i]), '.tif'), overwrite = TRUE)
    print('Done!')
  
  }

  print(paste0('Done ', year))  
  
}

# Apply th function
map(1982:2016, extract_mask)

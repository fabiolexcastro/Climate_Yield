
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data
root <- '//catalogue/BaseLineData_cluster04/GLOBAL/Climate/CHIRTS'
fles <- list.files(root, full.names = T, pattern = '.tif$')
shpf <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/base/MGN_DPTO_POLITICO.shp')
vars <- c('Tmin', 'Tmax')
yrss <- 1983:2016

# Function
extract_mask <- function(year, varb){
  
  # year <- 1983
  # varb <- 'Tmax'
  
  print(year)
  fle <- grep(year, fles, value = TRUE)
  fle <- grep(varb, fle, value = TRUE)
  
  rst <- list()
  
  for(i in 1:length(fle)){
    
    print(fle[i])
    rst[[i]] <- raster(fle[i])
    rst[[i]] <- raster::crop(rst[[i]], shpf)
    rst[[i]] <- raster::mask(rst[[i]], shpf)
    rst[[i]] <- rst[[i]] * 1
    writeRaster(rst[[i]], filename = paste0('./raster/chirts/colombia/', basename(fle[i]), '.tif'), overwrite = TRUE)
    print('Done!')
    
  }
  
  print(paste0('Done ', year))  
  
}

# Apply th function
library(foreach)
library(doSNOW)
cl <- makeCluster(12)
registerDoSNOW(cl)

foreach(k = 2:length(yrss), .verbose = TRUE) %dopar% {
  
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
  
  extract_mask(year = yrss[k], varb = 'Tmax')
  
}
  
  
lapply(2:length(yrss), function(k) extract_mask(year = yrss[k], varb = vars[1]))
lapply(1:length(yrss), function(k) extract_mask(year = yrss[k], varb = vars[2]))

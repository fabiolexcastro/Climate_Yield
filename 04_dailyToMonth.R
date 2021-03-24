
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data
fld <- list.files('./raster/zones_coffee', full.names = TRUE)
yrs <- 1993:2016

# Function
make_month <- function(yr){
  
  yr <- 2016
  
  fl <- grep(yr, fld, value = TRUE)
  fl <- list.files(fl, full.names = T)
  
  # Tmax
  cx <- map(1:12, function(k){
    
    print(k)
    mn <- ifelse(k < 10,  paste0('0', k), k)
    fs <- grep(paste0(yr, '.', mn, '.'), fl, value = TRUE)
    
    cx <- grep('Tmax', fs, value = TRUE)
    cx <- map(cx, raster)
    cx <- stack(cx)
    cx <- rasterToPoints(cx, spatial = FALSE) %>% as_tibble()
    cx <- as_tibble(cbind(cx[,1:2], value = apply(cx[,3:ncol(cx)], 1, 'mean')))
    cx <- rasterFromXYZ(cx)
    return(cx)
    
  })
  cx <- stack(cx)
  
  # Tmin
  cn <- map(1:12, function(k){
    
    print(k)
    mn <- ifelse(k < 10,  paste0('0', k), k)
    fs <- grep(paste0(yr, '.', mn, '.'), fl, value = TRUE)
    
    cn <- grep('Tmin', fs, value = TRUE)
    cn <- map(cn, raster)
    cn <- stack(cn)
    cn <- rasterToPoints(cn, spatial = FALSE) %>% as_tibble()
    cn <- as_tibble(cbind(cn[,1:2], value = apply(cn[,3:ncol(cn)], 1, 'mean')))
    cn <- rasterFromXYZ(cn)
    return(cn)
    
  })
  cn <- stack(cn)
  
  # CHIRPS
  cp <- map(1:12, function(k){
    
    print(k)
    mn <- ifelse(k < 10,  paste0('0', k), k)
    fs <- grep(paste0(yr, '.', mn, '.'), fl, value = TRUE)
    
    cp <- grep('chirps', fs, value = TRUE)
    cp <- map(cp, raster)
    cp <- stack(cp)
    cp <- rasterToPoints(cp, spatial = FALSE) %>% as_tibble()
    cp <- as_tibble(cbind(cp[,1:2], value = apply(cp[,3:ncol(cp)], 1, 'sum')))
    cp <- rasterFromXYZ(cp)
    return(cp)
    
  })
  cp <- stack(cp)
  
  Map('writeRaster', x = unstack(cx), filename = paste0('./raster/monthly/', yr, '_', 'tmax', '_', 1:12, '.tif'), overwrite = TRUE)
  Map('writeRaster', x = unstack(cn), filename = paste0('./raster/monthly/', yr, '_', 'tmin', '_', 1:12, '.tif'), overwrite = TRUE)
  Map('writeRaster', x = unstack(cp), filename = paste0('./raster/monthly/', yr, '_', 'prec', '_', 1:12, '.tif'), overwrite = TRUE)
  
}

# Apply the function
yrs <- 1992:2016
map(.x = yrs, .f = make_month)

library(foreach); library(doSNOW)
cl <- makeCluster(6); registerDoSNOW(cl)

foreach(i = 1:length(yrs), .verbose = TRUE) %dopar% {
  require(pacman)
  pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
  make_month(yr = yrs[i])
}

dne <- list.files('./raster/monthly')


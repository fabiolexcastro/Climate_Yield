
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

# Load data
fld <- list.files('./raster', full.names = TRUE)
zne <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/aggregate_cafe/cff_10k.shp')

# Function 
stacking <- function(year){
  
  # print(year)
  # year <- 1994
  
  # Reading chirts data
  cht <- list.files(paste0('./raster/chirts/', '/colombia'), full.names = T)
  cht <- grep(year, cht, value = TRUE)
  
  # Tmax and Tmin
  chx <- grep('Tmax', cht, value = TRUE)
  chx <- stack(chx)
  
  chx <- lapply(1:nlayers(chx), function(k){
    print(k)
    chx[[k]] %>% raster::crop(., zne) %>% raster::mask(., zne)
  })
  
  chx <- stack(chx)
  
  chn <- grep('Tmin', cht, value = TRUE)
  chn <- stack(chn)
  
  chn <- lapply(1:nlayers(chn), function(k){
    print(k)
    chn[[k]] %>% raster::crop(., zne) %>% raster::mask(., zne)
  })
  
  chn <- stack(chn)
  
  # Stacking tmax and tmins
  cht <- addLayer(chx, chn)
  
  # Reading CHIRPS data
  chp <- list.files(paste0('./raster/chirps', '/colombia'), full.names = T)
  chp <- grep(year, chp, value = TRUE)
  chp <- stack(chp)
  
  chp <- lapply(1:nlayers(chp), function(k){
    print(k)
    chp[[k]] %>% raster::crop(., zne) %>% raster::mask(., zne)
  })
  
  chp <- stack(chp)
  
  # Stacking Tmax, Tmin y Prec
  stk <- addLayer(cht, chp)

  Map('writeRaster', x = unstack(stk), filename = paste0('./raster/zones_coffee/', names(stk), '.tif'), overwrite = TRUE)
  print('done!')
}

yearss <- 1983:2011
map(.x = yearss, .f = stacking)


chk <- list.files('./raster/zones_coffee',full.names = TRUE)

by_year <- function(yr){
  
  # yr <- 2016
  
  print(yr)
  ch <- grep(paste0('.', yr, '.'), chk, value = TRUE)
  dir.create(paste0('./raster/zones_coffee/', yr))
  
  map(1:length(ch), function(k){
    
    file.rename(from = ch[k], to = paste0('./raster/zones_coffee/', yr, '/', basename(ch[k])))
    
  })
  
  print('Done!')
  
}


map(.x = 2012:2016, .f = by_year)


# 1990, 2009

# Check the results -------------------------------------------------------
years <- list.files('./raster/zones_coffee', full.names = TRUE)
files <- lapply(years, list.files)

yrs <- 1983:2016
yrs[which(lapply(files, length) == 0)]




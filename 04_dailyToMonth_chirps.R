
# Load libraries
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data
fld <- list.files('./raster/zones_coffee', full.names = TRUE)
yrs <- 1983:2016

# Function
make_month <- function(yr){
  
  yr <- 1996
  
  fl <- grep(yr, fld, value = TRUE)
  fl <- list.files(fl, full.names = T)
  
  
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
  
  Map('writeRaster', x = unstack(cp), filename = paste0('./raster/monthly/', yr, '_', 'prec', '_', 1:12, '.tif'), overwrite = TRUE)
  
}

# Apply the function
yrs <- 1984:2016
map(.x = yrs, .f = make_month)

# Check the results -------------------------------------------------------
fls <- list.files('./raster/monthly', full.names = TRUE, pattern = '.tif$')
dfm <- as_tibble(data.frame(year = str_sub(basename(fls), 1, 4), variable = str_sub(basename(fls), 6, 9), month = str_sub(basename(fls), 11, nchar(basename(fls)) - 4)))

dfm %>% 
  group_by(year, variable) %>% 
  summarize(count = n()) %>% 
  ungroup() %>% 
  tail()


rds <- list.files('./rds/etp')

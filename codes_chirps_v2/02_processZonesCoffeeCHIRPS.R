
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, foreach, doSNOW, future)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
extract_mask <- function(year){
  
  print(year)
  # year <- 2007
  fle <- grep(year, fles, value = TRUE)
  
  plan(cluster, workers = 30, gc = TRUE)
  system.time(expr = furrr::future_map(.x = 1:length(fle), .f = function(k){
    
    print(k)
    fle[k] %>% 
      raster %>% 
      raster::crop(., shpf) %>% 
      raster::mask(., shpf) %>% 
      writeRaster(., filename = paste0(out, basename(fle[k])), overwrite = TRUE)
    
    print('Done!')
    
  }))
  
  future:::ClusterRegistry('stop')
  gc(reset = TRUE)
  print('Process done')
  
}

# Load data ---------------------------------------------------------------
root <- './workspace/CHIRPS_v2/raster/chirps/colombia'
fles <- list.files(root, full.names = TRUE)
shpf <- readRDS(file = './workspace/Yield_SPEI/rds/base/pol_cff.rds')
out <- './workspace/CHIRPS_v2/raster/chirps/zones_coffee/'

# To get the years --------------------------------------------------------
years <- unique(str_sub(basename(fles), start = 13, end = 16))

# To apply the function ---------------------------------------------------
map(.x = 1:length(years), .f = function(k) extract_mask(year = years[k]))

# End ---------------------------------------------------------------------
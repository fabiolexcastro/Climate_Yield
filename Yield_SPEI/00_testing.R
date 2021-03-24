

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------

# Base data -----------------------------------
mpio <- readRDS(file = './rds/base/mps.rds')
cffe <- readRDS(file = './rds/base/pol_cff.rds')
prdc <- readRDS(file = './rds/base/prd.rds')
year <- paste0('.', 2007:2016, '.')

mpio_cffe <- raster::intersect(mpio, cffe)
mpio_cffe$NOM_MUNICI <- iconv(mpio_cffe$NOM_MUNICI, from = 'UTF-8', to = 'latin1')

# Climate data --------------------------------
path <- str_split(getwd(), pattern = '/workspace/')[[1]][1]
fles_prec <- paste0(path, '/', 'raster/chirps/colombia') %>% 
  list.files(., full.names = T, pattern = '.tif$') %>% 
  grep(paste0(year, collapse = '|'), ., value = TRUE)
fles_tmax <- paste0(path, '/', 'raster/chirts/colombia') %>% 
  list.files(., full.names = T, pattern = '.tif$') %>% 
  grep('Tmax', ., value = TRUE) %>% 
  grep(paste0(year, collapse = '|'), ., value = TRUE)
fles_tmin <- paste0(path, '/', 'raster/chirts/colombia') %>% 
  list.files(., full.names = T, pattern = '.tif$') %>% 
  grep('Tmin', ., value = TRUE)%>% 
  grep(paste0(year, collapse = '|'), ., value = TRUE)

extract_mask <- function(mpo){
  
  mpo <- 'PITALITO'
  
  print('To start')
  mpo <- mpio_cffe[mpio_cffe@data$NOM_MUNICI == mpo,]
  
  cl <- makeCluster(30)
  registerDoSNOW(cl)
  
  clm <- foreach(i = 1:length(fles_prec), .verbose = TRUE) %dopar% {
    
    # Load libraries
    require(pacman)
    pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)
    
    # Read raster
    ppt <- raster(fles_prec[i]) / 10
    tmx <- raster(fles_tmax[i])
    tmn <- raster(fles_tmin[i])
    
    # Get the date
    yr <- as.numeric(str_sub(basename(fles_tmax[i]), start = 6, end = 9))
    mn <- as.numeric(str_sub(basename(fles_tmax[i]), start = 11, end = 12))
    dy <- as.numeric(str_sub(basename(fles_tmax[i]), start = 14, end = 15))
    
    # Extract by mask
    ppt <- raster::crop(ppt, mpo) %>% raster::mask(., mpo)
    tmx <- raster::crop(tmx, mpo) %>% raster::mask(., mpo)
    tmn <- raster::crop(tmn, mpo) %>% raster::mask(., mpo)
    
    # Stacking a raster to table
    stk <- stack(ppt, tmx, tmn)
    tbl <- rasterToPoints(stk, spatial = FALSE) %>% 
      as_tibble() %>% 
      mutate(gid = 1:nrow(.)) %>% 
      gather(var, value, -gid, -x, -y) %>% 
      mutate(year = yr, month = mn, day = dy) %>% 
      dplyr::select(gid, x, y, year, month, day, value)
    print('Done!')
    return(tbl)
    
  }
  
  clm <- bind_rows(clm)
  saveRDS(object = clm, file = './rds/climate/')
  
}


readRDS(file= './rds/spei/spei_3_zones_coffee.rds')












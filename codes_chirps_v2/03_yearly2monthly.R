
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, foreach, doSNOW, future)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
get_table <- function(yr){
  
  # yr <- 2007
  
  print(paste0('To start ', yr))
  # lim <- cffe[cffe@data$NOM_MUNICI %in% mp,]
  fle <- grep(yr, fles, value = TRUE)
  stk <- raster::stack(fle)
  # stk <- raster::crop(stk, lim); stk <- raster::mask(stk, lim)
  tbl <- rasterToPoints(stk, spatial = FALSE)
  tbl <- as_tibble(tbl)
  gth <- tbl %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
  gth <- gth %>% mutate(year = str_sub(var, 13, 16), month = str_sub(var, 18, 19), day = str_sub(var, 21, 22))
  gth <- gth %>% mutate(year = as.numeric(year), month = as.numeric(month), day = as.numeric(day))
  gth <- gth %>% dplyr::select(gid, x, y, year, month, day, value)
   mnt <- gth %>% group_by(gid, x, y, year, month) %>% dplyr::summarise(value = sum(value, na.rm = TRUE)) %>% ungroup()
  print('Done!')
  return(mnt)
  
}


# Load data ---------------------------------------------------------------
fles <- list.files('./workspace/CHIRPS_v2/raster/chirps/zones_coffee', full.names = TRUE, pattern = '.tif$')
shpf <- readRDS(file = './workspace/Yield_SPEI/rds/base/pol_cff.rds')
mpio <- readRDS(file = '//dapadfs/workspace_cluster_9/Coffee_Cocoa2/PROJECTS/2021/SPEI_COFFEE/workspace/Yield_SPEI/rds/base/mps.rds')

# Intersection ------------------------------------------------------------
cffe <- raster::intersect(x = shpf, y = mpio)
cffe@data$NOM_MUNICI <- iconv(cffe@data$NOM_MUNICI, from = 'UTF-8', to = 'latin1')
mpios_vect <- cffe@data$NOM_MUNICI

# Years
head(fles); tail(fles)
years <- 1983:2020
dir.create('./workspace/CHIRPS_v2/rds/chirps/zones_coffee', recursive = TRUE)

# -------------------------------------------------------------------------
# Apply the function ------------------------------------------------------
# -------------------------------------------------------------------------
plan(cluster, workers = 30, gc = TRUE)
system.time(expr = rslt <- furrr::future_map(.x = 1:length(years), .f = function(k){
  print(k)
  rsl <- years[k] %>% get_table(yr = .)
}))

future:::ClusterRegistry('stop')
gc(reset = TRUE)
rslt <- bind_rows(rslt)
saveRDS(object = rslt, file = './workspace/CHIRPS_v2/rds/chirps/zones_coffee/zones_coffee_monthly.rds')

# Table to raster
toraster <- function(yr, mn){
  
  # yr <- 2007
  # mn <- 8
  
  rst <- mnt %>% filter(year == yr & month == mn) %>% dplyr::select(x, y, value) %>% rasterFromXYZ()
  out <- './workspace/CHIRPS_v2/raster/chirps/zones_coffee/monthly/'
  writeRaster(x = rst, filename = paste0(out, '/', 'prec_', yr, '_', mn, '.tif'))
  
}

lapply(1:length(years), function(k){
  lapply(1:12, function(j){
    print(years[k])
    print(j)
    toraster(yr = years[k], mn = j)
  })
})


# Pitalito ----------------------------------------------------------------
plan(cluster, workers = 30, gc = TRUE)
system.time(expr = rslt <- furrr::future_map(.x = 1:length(years), .f = function(k){
  print(k)
  rsl <- years[k] %>% get_table(yr = .)
}))

future:::ClusterRegistry('stop')
gc(reset = TRUE)
rslt <- bind_rows(rslt)
saveRDS(object = rslt, file = './workspace/CHIRPS_v2/rds/chirps/zones_coffee/pitalito_monthly.rds')

# El Aguila ---------------------------------------------------------------
plan(cluster, workers = 30, gc = TRUE)
system.time(expr = rslt <- furrr::future_map(.x = 1:length(years), .f = function(k){
  print(k)
  rsl <- years[k] %>% get_table(mp = 'EL AGUILA', yr = .)
}))

future:::ClusterRegistry('stop')
gc(reset = TRUE)
rslt <- bind_rows(rslt)
saveRDS(object = rslt, file = './workspace/CHIRPS_v2/rds/chirps/zones_coffee/el_aguila_monthly.rds')







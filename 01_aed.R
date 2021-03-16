

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, foreach, doSNOW, parallel)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
get_var <- function(vr, yr){
  
  # vr <- 'tmin'
  # yr <- 2007
  
  df <- tb %>% dplyr::select(x, y, vr, year, month)
  names(df)[3] <- 'value'
  df <- df %>% filter(year == yr)
  rs <- list()
  
  for(i in 1:12){
    
    rs[[i]] <- df %>% 
      filter(month == i) %>% 
      dplyr::select(x, y, value) %>% 
      rasterFromXYZ %>% 
      raster::crop(., zn) %>% 
      raster::mask(., zn) %>% 
      rasterToPoints() %>% 
      as_tibble() %>% 
      mutate(year = yr, month = i)
    
  }
  
  rs <- bind_rows(rs)
  print('Done!')
  return(rs)
  
}
extract_table <- function(tb, mp){
  
  # tb <<- spei_03
  # mp <<- 'PITALITO'
  
  print('Filtering')
  tb <<- tb %>% filter(year %in% years)
  
  print('Production zone')
  zn <<- mpio_cffe[mpio_cffe@data$NOM_MUNICI == mp,]
  
  print('Each variables filter to the municipality')
  tmax <- bind_rows(map(.x = 1:length(years), .f = function(k) get_var(vr = 'tmax', yr = years[k])))
  names(tmax)[3] <- 'tmax'
  tmin <- bind_rows(map(.x = 1:length(years), .f = function(k) get_var(vr = 'tmin', yr = years[k])))
  names(tmin)[3] <- 'tmin'
  prec <- bind_rows(map(.x = 1:length(years), .f = function(k) get_var(vr = 'prec', yr = years[k])))
  names(prec)[3] <- 'prec'
  spei <- bind_rows(map(.x = 1:length(years), .f = function(k) get_var(vr = 'spei', yr = years[k])))
  names(spei)[3] <- 'spei'
  ettp <- bind_rows(map(.x = 1:length(years), .f = function(k) get_var(vr = 'etp', yr = years[k])))
  names(ettp)[3] <- 'etp'
  
  rslt <- list(tmax, tmin, prec, spei, ettp) %>% purrr::reduce(.,inner_join, by = c('x', 'y', 'year', 'month'))
  print('Done!')
  return(rslt)
  
}

# Load data ---------------------------------------------------------------

# Base data -----------------------------------
mpio <- readRDS(file = './rds/base/mps.rds')
cffe <- readRDS(file = './rds/base/pol_cff.rds')
prdc <- readRDS(file = './rds/base/prd.rds')
years <- 2007:2016

mpio_cffe <- raster::intersect(mpio, cffe)
mpio_cffe$NOM_MUNICI <- iconv(mpio_cffe$NOM_MUNICI, from = 'UTF-8', to = 'latin1')

# Climate data --------------------------------
spei_03 <- readRDS(file = './rds/spei/spei_3_zones_coffee.rds') %>% filter(year %in% years)
spei_06 <- readRDS(file = './rds/spei/spei_6_zones_coffee.rds') %>% filter(year %in% years)
spei_12 <- readRDS(file = './rds/spei/spei_6_zones_coffee.rds') %>% filter(year %in% years)

# Apply the function ------------------------------------------------------
tb_pit <- extract_table(t = spei_03, mp = 'PITALITO')
tb_agu <- extract_table(t = spei_03, mp = 'EL AGUILA')

saveRDS(object = tb_pit, file = './rds/mpios_vars/tb_pit.rds')
saveRDS(object = tb_agu, file = './rds/mpios_vars/tb_agu.rds')



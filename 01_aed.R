

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------

# Base data -----------------------------------
mpio <- readRDS(file = './rds/base/mps.rds')
cffe <- readRDS(file = './rds/base/pol_cff.rds')
prdc <- readRDS(file = './rds/base/prd.rds')

mpio_cffe <- raster::intersect(mpio, cffe)
mpio_cffe$NOM_MUNICI <- iconv(mpio_cffe$NOM_MUNICI, from = 'UTF-8', to = 'latin1')

# Climate data --------------------------------
path <- str_split(getwd(), pattern = '/workspace/')[[1]][1]
fles_prec <- paste0(path, '/', 'raster/chirps/colombia') %>% 
  list.files(., full.names = T, pattern = '.tif$') 
fles_tmax <- paste0(path, '/', 'raster/chirts/colombia') %>% 
  list.files(., full.names = T, pattern = '.tif$')


extract_mask <- function(mpo){
  
  mpo <- 'PITALITO'
  
  print('To start')
  mpo <- mpio_cffe[mpio_cffe$NOM_MUNICI == mpo,]
  
  
}














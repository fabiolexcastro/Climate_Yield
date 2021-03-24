
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, readxl)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
pnt <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/cafe/coffee_v1.shp')
pol <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_coffeeColombia/shp/aggregate_cafe/cff_10k.shp')
mps <- shapefile('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/PROJECTS/2021/SPEI_COFFEE/shp/mpios_geo_ok.shp')
prd <- read_excel('./workspace/test_correlation/v1/tbl/production/EVA.xlsx')

# Tidy the table ----------------------------------------------------------
prd <- prd %>% 
  filter(CULTIVO == 'CAFE') %>% 
  dplyr::select(2:4, 7, 9:14) %>% 
  setNames(c('dpto', 'code', 'mpio', 'year', 'periodo', 'crop', 'harvested', 'prod', 'yield'))

# Save to rds file --------------------------------------------------------
dir.create('./rds/base')
saveRDS(object = pnt, file = './rds/base/pnt_cff.rds')
saveRDS(object = pol, file = './rds/base/pol_cff.rds')
saveRDS(object = mps, file = './rds/base/mps.rds')
saveRDS(object = prd, file = './rds/base/prd.rds')



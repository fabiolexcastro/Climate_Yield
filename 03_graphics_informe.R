

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, hrbrthemes, rgdal, rgeos, stringr, mapsf, sf, tidyverse, ggpubr, RColorBrewer, ggspatial)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
pnt <- readRDS(file = './workspace/Yield_SPEI/rds/base/pnt_cff.rds')
dpt <- readRDS('./workspace/Yield_SPEI/rds/base/mps.rds')
dpt <- aggregate(dpt, 'NOMBRE_DPT')
dpt <- st_as_sf(dpt)
dpt <- dpt %>% mutate(NOMBRE_DPT = iconv(NOMBRE_DPT, from = 'UTF-8', to = 'latin1'))
pnt <- st_as_sf(pnt)

dp2 <- dpt %>% filter(NOMBRE_DPT %in% c('CAUCA', 'NN')) %>% mutate(NOMBRE_DPT = 'CAUCA') %>% as(., 'Spatial') %>% aggregate(., 'NOMBRE_DPT') %>% st_as_sf()
dpt <- dpt %>% filter(!NOMBRE_DPT %in% c('CAUCA', 'NN')) %>% rbind(., dp2)
saveRDS(object = dpt, file = './workspace/Yield_SPEI/rds/base/dpt.rds')

wrl <- st_read('//dapadfs/workspace_cluster_9/Coffee_Cocoa2/_cocoa_cam/_data/_shp/_base/all_countries.shp')
wrl <- wrl %>% filter(CONTINENT %in% c('South America', 'North America'))
wrl <- wrl %>% filter(!NAME %in% c('United States'))
mf_map(wrl)
saveRDS(object = wrl, file = './workspace/Yield_SPEI/rds/base/countries.rds')

pol <- readRDS(file = './workspace/Yield_SPEI/rds/base/pol_cff.rds')
pol <- st_as_sf(pol)

# To make a nice map ------------------------------------------------------
gg_points <- ggplot() +
  geom_sf(data = wrl, fill = NA) +
  geom_sf(data = dpt, fill = 'grey90', color = 'grey10') +
  geom_sf(data = pnt, color = 'brown', size = 0.8) +
  geom_sf_text(data = dpt, aes(label = NOMBRE_DPT), size = 1.15, check_overlap = TRUE, color = 'grey40') +
  coord_sf(xlim = extent(pnt)[1:2], ylim = extent(pnt)[3:4]) +
  labs(x = 'Lon', y = 'Lat') + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 6, face = 'bold'),
        axis.title.y = element_text(size = 6, face = 'bold')) 

gg_polygn <- ggplot() +
  geom_sf(data = wrl, fill = NA) +
  geom_sf(data = dpt, fill = 'grey90', color = 'grey10') +
  geom_sf(data = pol, color = 'brown', fill = 'brown') +
  theme_tinyhand() +
  coord_sf(xlim = extent(pnt)[1:2], ylim = extent(pnt)[3:4]) +
  geom_sf_text(data = dpt, aes(label = NOMBRE_DPT), size = 1.15, check_overlap = TRUE, color = 'grey40') +
  labs(x = 'Lon', y = 'Lat') + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 6, face = 'bold'),
        axis.title.y = element_text(size = 6, face = 'bold')) 

gg <- ggpubr::ggarrange(gg_points, gg_polygn, nrow = 1, ncol = 2)
ggsave(plot = gg, filename =  './png/maps/base/coffee_points_polygon.png', units = 'in', width = 6, height = 5, dpi = 300)



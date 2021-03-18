

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, hrbrthemes, rgdal, rgeos, stringr, mapsf, sf, tidyverse, ggpubr, RColorBrewer, ggspatial)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
pnt <- readRDS(file = './workspace/Yield_SPEI/rds/base/pnt_cff.rds')
mps <- readRDS('./workspace/Yield_SPEI/rds/base/mps.rds')
dpt <- readRDS('./workspace/Yield_SPEI/rds/base/dpt.rds')
wrl <- readRDS('./workspace/Yield_SPEI/rds/base/countries.rds')
pol <- readRDS('./workspace/Yield_SPEI/rds/base/pol_cff.rds')

# Processing intersection -------------------------------------------------
sft <- st_intersection(st_as_sf(mps), st_as_sf(pol))
mf_map(sft)

pit <- sft %>% filter(NOM_MUNICI == 'PITALITO')
pt <- mps %>% st_as_sf(.) %>% filter(NOM_MUNICI == 'PITALITO')

agu <- sft %>% filter(NOM_MUNICI == 'EL AGUILA')
ag <- mps %>% st_as_sf(.) %>% filter(NOM_MUNICI == 'EL AGUILA')

mps <- st_as_sf(mps)
mps <- mps %>% mutate(NOM_MUNICI = iconv(NOM_MUNICI, from = 'UTF-8', to = 'latin1'))

# To make the graph
gg_pt <- ggplot() +
  geom_sf(data = pit, fill = 'brown', color = 'brown') + 
  geom_sf(data = pt, fill = NA, color = 'black') +
  geom_sf(data = mps, fill = NA) +
  coord_sf(xlim = extent(pt)[1:2], ylim = extent(pt)[3:4]) +
  geom_sf_text(data = mps, aes(label = NOM_MUNICI), size = 1.9) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 6, face = 'bold'),
        axis.title.y = element_text(size = 6, face = 'bold'),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        panel.grid = element_blank()) +
  labs(x = 'Lon', y = 'Lat')

gg_ag <- ggplot() +
  geom_sf(data = agu, fill = 'brown', color = 'brown') + 
  geom_sf(data = ag, fill = NA, color = 'black') +
  geom_sf(data = mps, fill = NA) +
  coord_sf(xlim = extent(ag)[1:2], ylim = extent(ag)[3:4]) +
  geom_sf_text(data = mps, aes(label = NOM_MUNICI), size = 1.9) +
  theme_bw() +
  theme(axis.title.x = element_text(size = 6, face = 'bold'),
        axis.title.y = element_text(size = 6, face = 'bold'),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 5),
        panel.grid = element_blank()) +
  labs(x = 'Lon', y = 'Lat')

gg <- ggarrange(gg_pt, gg_ag, nrow = 1, ncol = 2)
ggsave(plot = gg, filename = './png/maps/base/mpios.png', units = 'in', width = 7, height = 5, dpi = 300)




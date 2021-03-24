 
# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, classInt, rgeos, stringr, sf, tidyverse, SPEI, gtools, RColorBrewer)
g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
make_map <- function(yr){
  
  # yr <- 1984
  
  print(yr)
  rs <- spei %>% filter(year == yr)
  rs <- rs %>% mutate(clase = factor(clase, levels = 1:7))
  
  clr <- clrs$color
  
  lb <- rs %>% distinct(clase) %>% mutate(clase = as.numeric(clase)) %>% inner_join(., lbls, by = c('clase' = 'V3'))
  lb <- lb %>% mutate(labls = paste0(V1, ' to ', V2))
  clr
  cl <- inner_join(clrs, lb, by = c('class' = 'clase'))
  
  gg <- ggplot() +
    geom_tile(data = rs, aes(x = x, y = y, fill = clase)) +
    facet_wrap(.~month_abb) +
    scale_fill_manual(values = cl$color,
                      labels = cl$labls) +
    geom_sf(data = colm, fill = NA, color = '#D8D8D8') +
    coord_sf(ylim = c(0, 12)) +
    theme_bw() +
    theme(legend.position = 'bottom',
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = 'Lon', y = 'Lat', fill = 'SPEI')
  
  ggsave(plot = gg, filename = paste0('./png/maps/spei/scale_12/', yr, '.png'), width = 12, height = 9.5, dpi = 300)
  
  print('Done!')
  
}

# Load data ---------------------------------------------------------------
spei <- readRDS(file = './rds/spei/spei_zones_coffee.rds')
spei <- inner_join(spei, data.frame(month = 1:12, month_abb = month.abb), by = 'month')
spei <- spei %>% mutate(month_abb = factor(month_abb, levels = month.abb))
colm <- st_read('./shp/DPTO.shp')

spei %>% pull(spei) %>% na.omit() %>% as.numeric() %>% hist()
values <- spei %>% pull(spei)  
summary(values)

lbls <- as.data.frame(matrix(c(-1.5, -1.25, 1, -1.25, -1.0, 2, -1, 1, 3, 1, 1.25, 4, 1.25, 1.5, 5, 1.5, 1.75, 6, 1.75, 2, 7), ncol = 3, nrow = 7, byrow = T))
clrs <- data.frame(color = c('#d73027', '#fc8d59', 'grey', '#e0f3f8', '#91bfdb', '#4575b4'), class = 1:6)
num <- findInterval(values, lbls$V1, all.inside = TRUE)
spei <- spei %>% mutate(clase = num)

# Apply the function ------------------------------------------------------
year <- 1983:2016
map(.x = year, .f = make_map)

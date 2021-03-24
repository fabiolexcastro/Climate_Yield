
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, mapsf, tidyverse, foreach, doSNOW, future, zoo)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Functions to use --------------------------------------------------------
make_graph <- function(tb, gd, nm){
  
  # Proof
  # gd <- 1
  # tb <- pitl
  # nm <- 'Pitalito'
  
  print(paste0('To start ', nm))
  tb <- tb %>%
    filter(gid == gd) %>% 
    mutate(date = paste0(year, '-', ifelse(month < 10, paste0('0', month), month))) %>% 
    dplyr::select(gid, x, y, date, year, month, value) %>% 
    mutate(date = as.Date(as.yearmon(date)))
  
  print('To make the graph')
  gg <- ggplot(data = tb, aes(x = date, y = value)) +
    geom_line(size = 0.6, col = 'blue') +
    scale_x_date(date_breaks = '1 year', date_labels = c('%Y-%m')) +
    theme_ipsum() +
    theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
          axis.text.y = element_text(size = 10),
          axis.title.x = element_text(size = 10, face = 'bold'),
          axis.title.y = element_text(size = 10, face = 'bold')) +
    xlab('Fecha') +
    ylab('Precipitation (mm)') 
  
  out <- './workspace/CHIRPS_v2/png/graphs/precipitation'
  ggsave(plot = gg, filename = paste0(out, '/', nm, '_', gd, '.png'), units = 'in', width = 9, height = 6, dpi = 300)
  print('Done!')
  
}



# Load data ---------------------------------------------------------------
pitl <- readRDS('./workspace/CHIRPS_v2/rds/chirps/zones_coffee/pitalito_monthly.rds')
agla <- readRDS('./workspace/CHIRPS_v2/rds/chirps/zones_coffee/el_aguila_monthly.rds')

# Make graphs Pitalito and El Aguila ---------------------------------------
lapply(1:length(unique(pitl$gid)), function(k) make_graph(tb = pitl, gd = k, nm = 'Pitalito'))
lapply(1:length(unique(agla$gid)), function(k) make_graph(tb = agla, gd = k, nm = 'El Aguila'))

# Make general graph for the average --------------------------------------

# Pitalito
pitl_smm <- pitl %>% 
  group_by(year, month) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', ifelse(month < 10, paste0('0', month), month))) %>% 
  dplyr::select(date, year, month, value) %>% 
  mutate(date = as.Date(as.yearmon(date)))

gg_pit <- ggplot(data = pitl_smm, aes(x = date, y = value)) +
  geom_line(size = 0.6, col = 'blue') +
  scale_x_date(date_breaks = '1 year', date_labels = c('%Y-%m')) +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 10, face = 'bold')) +
  xlab('Fecha') +
  ylab('Precipitation (mm)') 

out <- './workspace/CHIRPS_v2/png/graphs/precipitation'
ggsave(plot = gg_pit, filename = paste0(out, '/', 'Pitalito', '_', 'avg', '.png'), units = 'in', width = 9, height = 6, dpi = 300)

# El Aguila
agla_smm <- agla %>% 
  group_by(year, month) %>% 
  dplyr::summarise(value = mean(value)) %>% 
  ungroup() %>% 
  mutate(date = paste0(year, '-', ifelse(month < 10, paste0('0', month), month))) %>% 
  dplyr::select(date, year, month, value) %>% 
  mutate(date = as.Date(as.yearmon(date)))

gg_agu <- ggplot(data = agla_smm, aes(x = date, y = value)) +
  geom_line(size = 0.6, col = 'blue') +
  scale_x_date(date_breaks = '1 year', date_labels = c('%Y-%m')) +
  theme_ipsum() +
  theme(axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 10, face = 'bold'),
        axis.title.y = element_text(size = 10, face = 'bold')) +
  xlab('Fecha') +
  ylab('Precipitation (mm)') 

ggsave(plot = gg_agu, filename = paste0(out, '/', 'El Aguila', '_', 'avg', '.png'), units = 'in', width = 9, height = 6, dpi = 300)

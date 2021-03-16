
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, hrbrthemes, rgdal, rgeos, zoo, stringr, sf, tidyverse, foreach, doSNOW, parallel, lubridate)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
order_table <- function(tb, nm){
  
  # Proof
  # tb <- pit
  # nm <- 'Pitalito'
  
  print('To start')
  tb <- tb %>% 
    distinct(x, y) %>% 
    mutate(gid = 1:nrow(.)) %>% 
    inner_join(., tb, by = c('x', 'y')) %>%  
    mutate(date = paste0(year, '-', ifelse(month < 10, paste0('0', month), month)),
           date = as.yearmon(date)) %>% 
    group_by(year, month, date) %>% 
    dplyr::summarise(tmax = mean(tmax, na.rm = TRUE),
                     tmin = mean(tmin, na.rm = TRUE),
                     prec = mean(prec, na.rm = TRUE),
                     spei = mean(spei, na.rm = TRUE),
                     etp = mean(etp, na.rm = TRUE)) %>% 
    ungroup()
  
  print('To gather the table')
  gt <- tb %>% gather(var, value, -year, -month, -date)
  lb <- data.frame(tipe = c('Temperatura (C)', 'Temperatura (C)', 'Precipitación-ETP (mm)', 'SPEI', 'Precipitación-ETP (mm)'), var = c('tmax', 'tmin', 'prec', 'spei', 'etp'))
  gt <- inner_join(gt, lb, by = 'var')
  
  print('To make the graph')
  gg <- ggplot(data = gt, aes(x = date, y = value, color = var)) +
    geom_line(size = 1.05) +
    # geom_smooth(se = FALSE, method = 'loess') +
    facet_wrap(.~ tipe, scales = 'free_y') +
    theme_ipsum() +
    labs(x = '', y = '', color = '') +
    theme(legend.position = 'top',
          axis.text.x = element_text(size = 9, face = 'bold', angle = 45, vjust = 0.5),
          axis.text.y = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 11, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 10, face = 'italic')) +
    ggtitle(label = paste0('Comportamiento variables climáticas para\n', nm))
  
  ggsave(plot = gg, filename = paste0('./png/graphs/', 'vars_', nm, '.png'), units = 'in', width = 11, height = 7, dpi = 300)
  print('Done!')
  return(list(gt, gg))
  
}

# Load data ---------------------------------------------------------------

# Climate data --------------------------------
pit <- readRDS('./rds/mpios_vars/tb_pit_03.rds')
agu <- readRDS('./rds/mpios_vars/tb_agu_03.rds')

# Base data -----------------------------------
mpio <- readRDS(file = './rds/base/mps.rds')
cffe <- readRDS(file = './rds/base/pol_cff.rds')
prdc <- readRDS(file = './rds/base/prd.rds')

# To get the graphics -----------------------------------------------------
gg_pit <- order_table(tb = pit, nm = 'Pitalito')
gg_agu <- order_table(tb = agu, nm = 'El Aguila')







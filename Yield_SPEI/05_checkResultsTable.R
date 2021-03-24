

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, hrbrthemes, rgdal, rgeos, stringr, mapsf, sf, rlang, tidyverse, ggpubr, RColorBrewer, ggspatial, zoo)

g <- gc(reset = TRUE)
rm(list = ls())

# Functions to use --------------------------------------------------------
order_table <- function(tb, nm){
  
  # print('Proof')
  # tb <- pit; nm <- 'Pitalito'
  
  print('To start')
  tb <- tb %>% 
    distinct(x, y) %>% 
    mutate(gid = 1:nrow(.)) %>% 
    inner_join(., tb, by = c('x', 'y')) %>%  
    mutate(date = paste0(year, '-', ifelse(month < 10, paste0('0', month), month)),
           date = as.yearmon(date)) 
  
  tb
  tb %>% filter(prec > 1 | prec < -1) %>% pull(year)
  
  summary(tb$prec)
  hist(tb$prec)
  
  tb <- tb %>% 
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
    ggtitle(label = paste0('Comportamiento SPEI (scale = 12) para\n', nm)) 
  
  # ggsave(plot = gg, filename = paste0('./workspace/Yield_SPEI/png/graphs/three_plots/', 'vars_', nm, '.png'), units = 'in', width = 11, height = 7, dpi = 300)
  print('Done!')
  return(gt)
  
}
make_graph_yield <- function(tb, nm){
  
  # Proof
  # tb <- pit_prd; nm <- 'El Aguila'
  
  gg <- ggplot(data = tb, aes(x = year, y = yield)) +
    geom_line(size = 1.2, col = 'red') +
    # geom_smooth(se = FALSE, method = 'loess') +
    theme_ipsum() +
    labs(x = 'Año', y = 'Rendimientos (Ton/Ha)') +
    theme(axis.text.x = element_text(size = 9, face = 'bold', angle = 45, vjust = 0.5),
          axis.text.y = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 11, face = 'bold', hjust = 0.5),
          legend.text = element_text(size = 10, face = 'italic')) +
    scale_x_continuous(limits = c(2007, 2019), breaks = seq(2007, 2019, 1))
  # ggsave(plot = gg, filename = paste0('./workspace/Yield_SPEI/png/graphs/rdto_', nm, '.png'), units = 'in', width  = 7, height = 5, dpi = 300)
  print('Done!')
  return(gg)
  
}

# Load data ---------------------------------------------------------------
prd <- read_csv('./workspace/Yield_SPEI/tbl/production.csv')
prd <- prd %>% setNames(c('dpto', 'code', 'mpio', 'cultivo', 'year', 'periodo', 'crop', 'harvested', 'prod', 'yield'))
prd <- prd %>% mutate(mpio = iconv(mpio, to = 'latin1'))

# Scale = 12
pit <- readRDS('./workspace/Yield_SPEI/rds/mpios_vars/tb_pit_12.rds')
pit <- pit %>% filter(year >= 2007 & year < 2017)
pit <- order_table(tb = pit, nm = 'Pitalito')

# Scale = 12
agu <- readRDS('./workspace/Yield_SPEI/rds/mpios_vars/tb_agu_12.rds')
agu <- agu %>% filter(year >= 2007 & year < 2017)
agu <- order_table(tb = agu, nm = 'El Aguila')

# Productividad -----------------------------------------------------------

# Pitalito 
pit_prd <- prd %>% filter(mpio == 'PITALITO')
gg_pit_prd <- make_graph_yield(tb = pit_prd)

pit <- pit %>%
  filter(var == 'spei') %>% 
  dplyr::select(-date, -var, -tipe) %>% 
  group_by(year) %>% 
  dplyr::summarise(spei = mean(value, na.rm = TRUE)) %>% 
  inner_join(., pit_prd, by = 'year')

pit_gth <- pit %>% dplyr::select(year, -periodo, yield, spei)
pit_gth <- pit_gth %>% gather(var, value, -year)

# Logaritmic
pit <- pit %>% mutate(yield_log = log10(yield))
pit_gth <- pit %>% dplyr::select(year, -periodo, yield_log, spei)
pit_gth <- pit_gth %>% gather(var, value, -year)

gg <- ggplot(data = pit_gth, aes(x = year, y = value, color = var)) +
  # geom_line() +
  geom_smooth(se = FALSE, method = 'loess') +
  theme_ipsum() +
  labs(x = 'Año', y = 'Valor') +
  theme(axis.text.x = element_text(size = 9, face = 'bold', angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 9, face = 'bold'),
        plot.title = element_text(size = 11, face = 'bold', hjust = 0.5),
        legend.text = element_text(size = 10, face = 'italic'),
        legend.position = 'top') +
  scale_x_continuous(limits = c(2007, 2016), breaks = seq(2007, 2016, 1)) +
  labs(color = 'Variable') 

ggsave(plot = gg, filename = './workspace/Yield_SPEI/png/graphs/both_vars/yield_spei_pitalito_log.png', units = 'in', width = 7, height = 5, dpi = 300)

# El Aguila
agu_prd <- prd %>% filter(mpio == 'EL AGUILA')
gg_agu_prd <- make_graph_yield(tb = pit_prd)

agu <- agu %>%
  filter(var == 'spei') %>% 
  dplyr::select(-date, -var, -tipe) %>% 
  group_by(year) %>% 
  dplyr::summarise(spei = mean(value, na.rm = TRUE)) %>% 
  inner_join(., agu_prd, by = 'year')

agu_gth <- agu %>% dplyr::select(year, -periodo, yield, spei)
agu_gth <- agu_gth %>% gather(var, value, -year)

# Logaritimic
agu <- agu %>% mutate(yield_log = log10(yield))
agu_gth <- agu %>% dplyr::select(year, -periodo, yield_log, spei)
agu_gth <- agu_gth %>% gather(var, value, -year)

gg <- ggplot(data = agu_gth, aes(x = year, y = value, color = var)) +
  # geom_line() +
  geom_smooth(se = FALSE, method = 'loess') +
  theme_ipsum() +
  labs(x = 'Año', y = 'Valor') +
  theme(axis.text.x = element_text(size = 9, face = 'bold', angle = 45, vjust = 0.5),
        axis.text.y = element_text(size = 9, face = 'bold'),
        plot.title = element_text(size = 11, face = 'bold', hjust = 0.5),
        legend.text = element_text(size = 10, face = 'italic'),
        legend.position = 'top') +
  scale_x_continuous(limits = c(2007, 2016), breaks = seq(2007, 2016, 1)) +
  labs(color = 'Variable') 

ggsave(plot = gg, filename = './workspace/Yield_SPEI/png/graphs/both_vars/yield_spei_elaguila_log.png', units = 'in', width = 7, height = 5, dpi = 300)







# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, SPEI, gtools)
g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
fles <- list.files('./raster/monthly', full.names = TRUE)
year <- 1983:2016
yr <- 2016
fles <- fles[-grep('prec', fles, value = FALSE)]
prec <- list.files('./workspace/CHIRPS_v2/raster/chirps/zones_coffee/monthly', full.names = TRUE)
fles <- c(fles, prec)

# Functions to use --------------------------------------------------------
calc_etp <- function(yr){
  
  print(yr)
  fle <- grep(yr, fles, value = TRUE)
  fle <- mixedsort(fle)
  stk <- stack(fle)
  names(stk)[25:36] <- paste0('X', yr, '_prec_', 1:12)
  tbl <- rasterToPoints(stk, spatial = FALSE)
  tbl <- tbl %>% as_tibble(.) %>% mutate(gid = 1:nrow(.)) %>% gather(var, value, -gid, -x, -y)
  tbl <- tbl %>% mutate(variable = str_sub(var, 7, 10), year = as.numeric(str_sub(var, 2, 5)), month = as.numeric(str_sub(var, 12, nchar(var))))
  tbl <- tbl %>% dplyr::select(-var) %>% spread(variable, value)
  
  gds <- length(sort(unique(tbl$gid)))
  
  rsl <- map(1:gds, function(k){
    
    tb <- tbl %>% filter(gid == k)
    lt <- unique(tb$y)
    tb <- tb %>% mutate(prec = prec)
    tb <- tb %>% mutate(etp = as.numeric(hargreaves(Tmi = tmin, Tmax = tmax, Pre = prec, lat = lt)))
    tb <- tb %>% mutate(bal = prec - etp)
    print('Done!')
    return(tb)
    
  })
  
  rsl <- bind_rows(rsl)
  saveRDS(object = rsl, file = paste0('./rds/etp/etp_', yr, '.rds'))
  print('Done!')
  
}
calc_spei <- function(gd){
  
  # gd <- 1
  
  rs <- etp %>% filter(gid == gd)

  sp <- spei(rs[,10], 12)
  sp <- sp$fitted
  sp <- as.numeric(sp)
  rs <- rs %>% mutate(spei = sp)
  print('Done!')
  return(rs)
  
}

# To calculate the ETP ----------------------------------------------------|
year <- 1983:2016
map(.x = year, .f = calc_etp)

# To calculate the SPEI ---------------------------------------------------
etp <- list.files('./rds/etp', full.names = T, pattern = '.rds$')
etp <- grep('etp', etp, value = TRUE)
etp <- map(.x = etp, .f = readRDS)
etp <- bind_rows(etp)
gds <- max(unique(etp$gid))

rsl <- map(.x = 1:gds, .f = calc_spei)
rsl <- bind_rows(rsl)
dir.create('./rds/spei')
saveRDS(object = rsl, file = './rds/spei/spei_12_zones_coffee_v2.rds')

tst <- rsl %>% filter(year == 2011 & gid == 1)

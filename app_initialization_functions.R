## Functions that run when app starts 
## valid(): for checking if object exists
## load.data(): loads wolverines.gpkg, initializes study area boundary
## update.inputs(): updates inputs that rely on data from wolverines.gpkg

# This is similar to isTruthy, but tailored to shiny
valid <- function(x) {
  if (
    is.null(x) |
    is.na(x) |
    x == '' |
    x == 'NA' |
    x == 'NULL' |
    length(x) == 0
  ) return(FALSE) 
  else return(TRUE)
}

# load data into reactiveValues object
# This is essentially a list that can be passed into other functions
load.data <- function() {
  # initialize study area boundary
  lats <- c(64.4, 64.4, 63.15, 63.15)
  lons <- c(-141, -137.1, -137.1, -141)
  df <- data.frame(lon = lons, lat = lats)
  
  study_boundary_init = st_as_sf(df, coords = c('lon', 'lat'), crs = 4326) %>%
    #st_transform(3578) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast('POLYGON')
  
  data <- reactiveValues(
    grid = st_read('www/wolverines.gpkg', 'grids', quiet = T) %>% st_transform(4326),
    linear = st_read('www/wolverines.gpkg', 'linear_features', quiet = T) %>% st_transform(4326),
    areal = st_read('www/wolverines.gpkg', 'areal_features', quiet=T) %>% st_transform(4326),
    factors = st_read('www/wolverines.gpkg', 'survey_factors', quiet=T) %>% st_transform(4326),
    # Trondek Hwechin Traditional Territory
    thtt = st_read('www/wolverines.gpkg', 'th_trad_territ', quiet = T) %>% st_transform(4326),
    # And settlement lands
    settlement = st_read('www/wolverines.gpkg', 'th_settlement_land', quiet = T) %>% st_transform(4326),
    study_boundary = study_boundary_init,
    clicklist = list()
  )
  
  return(data)
}

# server functions 
# update inputs 
update.inputs <- function(input, session, data) {
  # print('in update inputs')
  updateSelectInput(inputId = 'inv',
                    choices = names(data$factors)[4:24],
                    selected = 'merge100_pct')
  
  updateSelectInput(inputId = 'factors',
                    choices = names(data$factors)[4:24],
                    selected = c('merge100_pct','elev_median','elev_sd','forest_pct',
                                 'water_pct')
  )
}

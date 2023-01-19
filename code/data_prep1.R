# Import, clip, and project key datasets
# PV 2023-01-18

library(sf)
library(dplyr)

setwd('wolverines')
dropbox <- 'C:/Users/PIVER37/Dropbox (BEACONs)/wolverines/data/'

# Read the 707 ~27km2 grids that were selected in ArcMap
grid <- st_read('data/thtt_boreal_cordillera.gdb', 'grids750') %>%
    mutate(id=row_number(), Shape_Length=NULL, Shape_Area=NULL) %>%
    rename(grid_m2=area_m2)
bnd <- st_union(grid)
st_write(bnd, 'www/wolverines.gpkg', 'bnd', delete_dsn=T, delete_layer=T)
st_write(grid, 'www/wolverines.gpkg', 'grids', delete_layer=T)

# Extract areal disturbances
area <- st_read('data/yg_disturbances.gpkg', "SD_Polygon") %>%
    st_intersection(st_union(bnd)) %>%
    st_cast('MULTIPOLYGON')
area <- mutate(area, Area_m2=st_area(area))
st_write(area, 'www/wolverines.gpkg', 'areal_features', delete_layer=T)

# Buffer areal features by 100m and 500m
area100 <- st_union(st_buffer(area, 100)) %>% 
    st_intersection(bnd)
st_write(area100, 'www/wolverines.gpkg', 'areal_features_100m', delete_layer=T)
area500 <- st_union(st_buffer(area, 500)) %>% 
    st_intersection(bnd)
st_write(area500, 'www/wolverines.gpkg', 'areal_features_500m', delete_layer=T)

# Extract linear disturbances
line <- st_read('data/yg_disturbances.gpkg', "SD_Line") %>%
    st_intersection(st_union(bnd)) %>%
    st_cast('MULTILINESTRING')
line <- mutate(line, Length_m=st_length(line))
st_write(line, 'www/wolverines.gpkg', 'linear_features', delete_layer=T)

# Buffer linear features by 100m and 500m
line100 <- st_union(st_buffer(line, 100)) %>%
    st_intersection(bnd) # clip to boundary
st_write(line100, 'www/wolverines.gpkg', 'linear_features_100m', delete_layer=T)
line500 <- st_union(st_buffer(line, 500)) %>%
    st_intersection(bnd) # clip to boundary
st_write(line500, 'www/wolverines.gpkg', 'linear_features_500m', delete_layer=T)

# Add TH traditional territory and settlement lands
th1 <- st_read('data/wolverines_v1.gpkg', 'th_trad_territ')
st_write(th1, 'www/wolverines.gpkg', 'th_trad_territ', delete_layer=T)
th2 <- st_read('data/wolverines_v1.gpkg', 'th_settlement_land')
st_write(th2, 'www/wolverines.gpkg', 'th_settlement_land', delete_layer=T)

# Add trapping concessions
trap <- st_read('data/wolverines_v1.gpkg', 'trapping_concessions')
st_write(trap, 'www/wolverines.gpkg', 'trapping_concessions', delete_layer=T)

# Add placer claims (Placer_Claims_50k.gdb, Placer_Claims_50k)
pc2 <- st_read('data/yt_placer_claims_50k.gpkg') %>%
    st_intersection(bnd) %>%
    st_cast('MULTIPOLYGON')
st_write(pc2, 'www/wolverines.gpkg', 'placer_claims', delete_layer=T)

# Add quartz claims (has geometry problems) (Quartz_Claims_50k.gdb, Quartz_Claims_50k)
qc2 <- st_read('data/yt_quartz_claims_50k.gpkg')
qc2 <- st_as_sfc(qc2) %>%
    lwgeom::lwgeom_make_valid() %>%
    st_as_sf() %>%
    st_intersection(bnd) %>%
    st_cast('MULTIPOLYGON')
st_write(qc2, 'www/wolverines.gpkg', 'quartz_claims', delete_layer=T)

# Add recent fires (1980-2021; Emergency_Management/Fire_History.gdb; Fire_History)
fires2 <- st_read('data/yt_fire_history.gpkg') %>%
    filter(FIRE_YEAR>=1980 & FIRE_YEAR<=2022) %>%
    select(FIRE_ID, FIRE_YEAR)
fires2 <- st_as_sfc(fires2) %>%
    lwgeom::lwgeom_make_valid() %>%
    st_as_sf() %>%
    st_intersection(bnd) %>%
    st_cast('MULTIPOLYGON')
st_write(fires2, 'www/wolverines.gpkg', 'recent_fires', delete_layer=T)

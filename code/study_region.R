library(sf)
library(dplyr)

new_grids <- st_read('wolverines/www/wolverines_750cells.gpkg', 'grids')
new_grids <- st_read('wolverines_extra/grids530.shp')
new_factors <- st_read('wolverines/www/new_region.gdb', 'survey_factors')
new_bnd <- st_union(new_grids)

st_write(new_grids,'wolverines/www/wolverines.gpkg', 'bnd', delete_layer=T)
st_write(new_grids,'wolverines/www/wolverines.gpkg', 'grids', delete_layer=T)
st_write(new_factors,'wolverines/www/wolverines.gpkg', 'survey_factors', delete_layer=T)

st_layers('wolverines/www/wolverines.gpkg')

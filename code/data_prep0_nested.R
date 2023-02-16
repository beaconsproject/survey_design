# Create nested grid inside each NTS grid, then merge and project.
# n=5 = 26.8km2; n=10 = 6.7km2; n=13 ~ 2km2 (grid sizes change with lat long)
# n=10 is 1:22750 while n=13 is 1:17500
# PV 2023-01-08 (adapted from ME)

library(sf)
library(dplyr)

nx <- 13 # number of nested grids e.g., 10 x 10
output <- 'data/fda9_nts_n13.shp'

# Boundary to select cells
bnd <- st_read('C:/Users/PIVER37/Documents/gisdata/123/kdtt.gdb','fda9')

# Read NTS grid and project to latlong
#nts <- st_read('C:/Users/PIVER37/Documents/gisdata/YT/Reference/NTS_Index_50k.gdb', 'NTS_Index_50k')
#nts <- st_read('C:/Users/PIVER37/Documents/gisdata/canada/nts/nts_snrc_50k.shp')
#nts <- st_read('C:/Users/PIVER37/Documents/gisdata/KDTT/kdtt_nts50k.shp')
#nts <- st_read('C:/Users/PIVER37/Documents/gisdata/123/boreal_cordillera.gdb','nts50k_buf50k')
nts <- st_read('C:/Users/PIVER37/Documents/gisdata/123/kdtt.gdb','fda9_nts')
nts = st_read('code/shp/hbasins12.shp')
nts_xy <- st_transform(nts, 4326)

# Create n x n grid within each NTS grid (Yukon version)
#lst <- list()
#for(i in nts_xy$NTS_MAP50K){
#  nts_i <- nts_xy[nts$NTS_MAP50K == i,] # subset nts
#  grd_i_sfc <- st_make_grid(nts_i, n = 5) # make smaller grids nested in nts
#  grd_i <- st_sf(nts = i, geometry = grd_i_sfc) # convert geometries to sf object
#  lst <- append(lst, list(grd_i))
#}

# Create n x n grid within each NTS grid (Canada version)
lst <- list()
for(i in nts_xy$NTS_SNRC){
  nts_i <- nts_xy[nts$NTS_SNRC == i,] # subset nts
  grd_i_sfc <- st_make_grid(nts_i, n = nx) # make smaller grids nested in nts
  grd_i <- st_sf(nts = i, geometry = grd_i_sfc) # convert geometries to sf object
  lst <- append(lst, list(grd_i))
}

# Merge and project back to Yukon Albers
full_grid <- do.call(rbind, lst)
full_grid_prj <- st_transform(full_grid, st_crs(nts))
full_grid_prj <- mutate(full_grid_prj, area_m2=st_area(full_grid_prj))

#st_write(full_grid_prj, 'data/boreal_cordillera_buf50k_nts_n5.shp')
grid <- full_grid_prj[bnd,]
st_write(grid, output, delete_layer=TRUE)

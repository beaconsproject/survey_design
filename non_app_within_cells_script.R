library(sf)
library(dplyr)

# Read data
bnd = st_read('www/wolverines.gpkg', 'bnd', quiet=T) %>%
    st_union()
grid = st_read('www/wolverines.gpkg', 'grids', quiet=T)
line100 = st_read('www/wolverines.gpkg', 'linear_features_100m', quiet=T) %>%
    st_intersection(bnd)
poly100 = st_read('www/wolverines.gpkg', 'areal_features_100m', quiet=T) %>%
    st_intersection(bnd)

# Merge 100-m buffered linear and polygonal features
merge100 = st_union(line100, poly100)

# Buffer out to 1000m
merge1000 = st_buffer(merge100, 900)

# Take the difference to get 100-1000m annulus
annulus = st_difference(merge1000, merge100)

# Intersect with grid to assign grid IDs to annulus segments
grid_annulus = st_intersection(grid, annulus) %>%
    st_cast('MULTIPOLYGON')

# Select 3 random sites from anywhere in each cell
n1 <- st_as_sf(terra::spatSample(terra::vect(grid), 3, strata='id'))

# Select 3 random sites from areas that are between 100-1000m of disturbances
n2 <- st_as_sf(terra::spatSample(terra::vect(grid_annulus), 3, strata='id'))

######################################################################################
# THE NEXT LINES OF CODE WILL REQUIRED THE CELLS THAT WERE RANDOMLY SELECTED
# cluster1 = 40 cells, cluster2a = 30 cells, cluster2b = 30 cells, cluster3 = 40 cells

# Cluster 1 - join the 40 cells in this cluster to n1
#cluster1_n1 <- left_join(cluster1, n1)

# Cluster 2a - join 30/60 cells in this cluster to n1
#cluster2a_n1 <- left_join(cluster2a, n1)

# Cluster 2b - join 30/60 cells in this cluster to n2
#cluster2b_n2 <- left_join(cluster2b, n2)

# Cluster 3 - join the 40 cells in this cluster to n2
#cluster3_n1 <- left_join(cluster3, n2)

# Last step: combine the clusters
#full_sample <- bind_rows(cluster1_n1, cluster2a_n1, cluster2b_n2, cluster3_n2)
######################################################################################
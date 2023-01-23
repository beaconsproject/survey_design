library(sf)
library(dplyr)


lyrs <- st_layers('wolverines/www/wolverines.gpkg')

for (i in 1:length(lyrs[[1]])) {
    print(lyrs[[1]][i])
    x <- st_read('wolverines/www/wolverines.gpkg', lyrs[[1]][i])
    st_write(x, paste0('wolverines/shp/',lyrs[[1]][i],'.shp'), delete_layer=T)
}

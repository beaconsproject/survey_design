library(sf)
library(dplyr)

factors <- st_read('wolverines/www/wolverines.gpkg', 'survey_factors')
x <- factors %>%
    mutate(
      placer_pct=ifelse(is.na(placer_pct),0,placer_pct),
      quartz_pct=ifelse(is.na(quartz_pct),0,quartz_pct),
      recent_fires_pct=ifelse(is.na(recent_fires_pct),0,recent_fires_pct),
      area500_pct=ifelse(is.na(area500_pct),0,area500_pct),
      line500_pct=ifelse(is.na(line500_pct),0,line500_pct),
      merge100_pct=ifelse(is.na(merge100_pct),0,merge100_pct),
      water_pct=ifelse(is.na(water_pct),0,water_pct),
      forest_pct=ifelse(is.na(forest_pct),0,forest_pct),
      wetland_pct=ifelse(is.na(wetland_pct),0,wetland_pct)
    )
 
y <- select(x, merge100_pct, id, grid_m2) %>% st_drop_geometry()
clust <- kmeans(scale(y), 2)$cluster
x <- x %>%
    filter(id %in% y$id) %>%
    mutate(clusters = clust)
x <- mutate(x, clusters=ifelse(!merge100_pct>0, 0, clusters))
table(x$clusters)

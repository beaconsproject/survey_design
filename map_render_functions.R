## Functions for rendering map and adding features to it
## 
## render.map1(): builds map
## map.selected.cells(): after cells are randomly selected, adds those selections 
##    to map
## modify.study.boundary(): modifies study boundary according to polygon drawn on
##    map by user
## update.transparency(): changes map transparency; no longer in use

render.map1 <- function(input, output, session, data) {
  if(!valid(input$inv)) {
    print('returning from render.map1 early')
    return()
  } 
  # print('in render.map1')
  output$map1 <- renderLeaflet({
    # Set color palette based on selected characteristics
    attrib <- pull(data$factors, input$inv)
    if (input$inv %in% c('benchmark_pct','forest_pct')) {
      #pal <- colorQuantile("YlGn", domain=attrib, n=5, na.color="transparent")
      pal <- colorNumeric("YlGn", domain=attrib, na.color="transparent")
    } else if (input$inv %in% c('water_pct','wetland_pct')) {
      #pal <- colorQuantile("PuBu", domain=attrib, n=5, na.color="transparent")
      pal <- colorNumeric("PuBu", domain=attrib, na.color="transparent")
    } else if (input$inv %in% c('quartz_pct','placer_pct','settlements_pct')) {
      #pal <- colorQuantile("YlGreysGn", domain=attrib, n=5, na.color="transparent")
      pal <- colorNumeric("Greys", domain=attrib, na.color="transparent")
    } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
      #pal <- colorQuantile("-BrBG", domain=attrib, n=5, na.color="transparent")
      pal <- colorNumeric("BrBG", domain=attrib, na.color="transparent")
    } else {
      pal <- colorQuantile("Reds", domain=attrib, n=5, na.color="transparent")
      pal <- colorNumeric("Reds", domain=attrib, na.color="transparent")
    }
    
    #map_bounds <- study_boundary %>% st_bbox() %>% as.character()
    bb <- st_bbox(data$factors)
    m <- leaflet() %>% addTiles() %>% 
      fitBounds(as.numeric(bb$xmin),as.numeric(bb$ymin),as.numeric(bb$xmax),as.numeric(bb$ymax)) %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      #addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery")
    # If the user clicks the button to generate clusters
    if (input$clustButton) {
        groups <- pull(data$clusters, clusters)
        #pal <- colorBin("RdYlBu", domain=data$factors[input$inv], bins=1:input$clusters)
        pal <- colorFactor("RdYlBu", domain=groups, n=input$clusters)
        m <- m %>% addPolygons(data=data$clusters, fillColor=~pal(clusters), fillOpacity=0.8, stroke = FALSE, group='Clusters') %>%
            addLegend(pal=pal, values=groups, opacity=1, title="Clusters")
    } else {
        m <- m %>% addPolygons(data=data$factors, fillColor=~pal(attrib), fillOpacity=1, stroke=F, group=input$inv) %>%
            addLegend(pal=pal, values=attrib, opacity=1, title=input$inv)
    }
    # If the user clicks the button to generate random samples
    if (input$goButton) {
        # simple random sample
        n1rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n1), 3, strata='id'))
        # stratified random sample
        n2rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n2), 3, strata='id'))
        m <- m %>% addPolygons(data=data$n2, fill=F, color='blue', weight=3, opacity=1, group='Stratified random') %>%
            addCircleMarkers(data=n2rnd3, radius=1, color='black', weight=3, fillOpacity=1, group="Camera traps (stratified)") %>%
            addPolygons(data=data$n1, fill=F, color='red', weight=3, opacity=1, group='Simple random') %>%
            addCircleMarkers(data=n1rnd3, radius=1, color='black', weight=3, fillOpacity=1, group="Camera traps (simple)")
    }
    # Base map
    #lup_pal <- colorFactor("RdYlBu", domain=data$lup, n=7)
    lup_pal <- c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#ffff33','#a65628')
    m <- m %>% addPolygons(data=data$grid, color='black', fill=F, weight=1, group='Grid')
      if (input$disturb) {
          m <- m %>% addPolylines(data=data$linear, color='black', weight=1, group='Linear features') %>%
          addPolygons(data=data$areal, color='red', weight=1, group='Areal features')
      }
      m <- m %>% addPolygons(data=data$thtt, fill=F, color='black', weight=1, group='TH traditional territory') %>%
      addPolygons(data=data$lup, fill=T, fillColor=lup_pal, fillOpacity=0.5, weight=1, group='Dawson LUP') %>%
      addPolygons(data=data$settlement, weight=1, group='Settlement lands') %>%
      addPolygons(data=data$study_boundary, fill=F, weight=1, color='black', group='Study boundary') %>%
      addLayersControl(position = "topleft",
                       baseGroups=c("Esri.NatGeoWorldMap", "Esri.WorldImagery"),
                       overlayGroups = c('Camera traps (simple)','Simple random','Camera traps (stratified)','Stratified random','Clusters',input$inv,'Grid','Linear features','Areal features','TH traditional territory','Settlement lands','Study boundary','Dawson LUP'),
                       options = layersControlOptions(collapsed=TRUE)) %>%
      hideGroup(c("Areal disturbances","Linear disturbances", "Areal features","Linear features","Elevation", "Stratified random","TH traditional territory", "Settlement lands","Camera traps (stratified)","Dawson LUP"))
  })
}

map.selected.cells <- function(input, output, session, data) {
 # add randomly selected cells
 print('in map.selected.cells')
 leafletProxy('map1', session, {
    n1rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n1), 3, strata='id'))
    n2rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n2), 3, strata='id'))
    addPolygons(data$n2, fill=F, color='yellow', weight=2, group='Stratified random') %>%
    addMarkers(n2rnd3, group="Camera traps (stratified)") %>%
    addPolygons(data$n1, group='Simple random') %>%
    addMarkers(n1rnd3, group="Camera traps (simple)")
 })
}

modify.study.boundary <- function(input, output, session, data) {
 print('click')
 if (length(data$clicklist) ==4) {
   data$clicklist <- list()
 }

 click <- input$map1_click

 data$clicklist[[length(data$clicklist) + 1]] <- click

 if (length(data$clicklist) == 4) {
   lats <- c(data$clicklist[[1]]$lat,
             data$clicklist[[2]]$lat,
             data$clicklist[[3]]$lat,
             data$clicklist[[4]]$lat)
   lons <- c(data$clicklist[[1]]$lng,
             data$clicklist[[2]]$lng,
             data$clicklist[[3]]$lng,
             data$clicklist[[4]]$lng)

   df <- data.frame(lon = lons, lat = lats)

   study_boundary_mod = st_as_sf(df, coords = c('lon', 'lat'), crs = 4326) %>%
     st_transform(3578) %>%
     summarise(geometry = st_combine(geometry)) %>%
     st_cast('POLYGON')

   data$study_boundary <- study_boundary_mod

   leafletProxy('map1',
             x = {tm_remove_layer(1000) +
                 tm_shape(data$study_boundary) + tm_borders(lwd = 2,
                                                            group='Study boundary')})
 }

 return(data)
}


# update.transparency <- function(input, session, data) {
#   # shinybusy::add_busy_spinner()
#   observeEvent(input$alpha, ignoreInit = T, handlerExpr = {
#     print('in update.transparency')
#     # Couldn't get pal to be within the scope of this function so I just copied
#     # and pasted it 
#     if (input$inv == 'forest_pct') {
#       pal <- 'YlGn'
#     } else if (input$inv %in% c('water_pct','wetland_pct')) {
#       pal <- 'PuBu'
#     } else if (input$inv %in% c('quartz_pct','placer_pct')) {
#       pal <- 'Greys'
#     } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
#       pal <- '-BrBG'
#     } else {
#       pal <- 'Reds'
#     }
#     
#     if (input$clustButton > 0) {
#       print('clust button clicked')
#       # create new layers for clusters/factors with alpha specified
#       m1 <- tm_shape(data$clusters) + tm_fill('clusters', palette = 'Set1',
#                                               alpha = input$alpha,
#                                               group = 'Clusters')
#     } else {
#       print('in else')
#       m1 <- tm_shape(data$factors) + tm_fill(input$inv, palette = pal, 
#                                              style = input$style,
#                                              alpha = input$alpha,
#                                              colorNA = NULL, 
#                                              group = input$inv,
#                                              title = input$inv)
#     }
#     m.linareal <- tm_shape(data$linear) + tm_lines(col = 'black',
#                                                    alpha = input$alpha,
#                                                    group = 'Linear features') +
#       tm_shape(data$areal) + tm_fill(col = 'red',
#                                      alpha = input$alpha,
#                                      group = 'Areal features')
#     tmapProxy('map1',
#               x = {tm_remove_layer(500) +
#                   tm_remove_layer(560) +
#                   tm_remove_layer(570) +
#                   m.linareal +
#                   m1}
#     ) # tmapProxy 
#   }) # observeEvent
#   # shinybusy::remove_modal_spinner()
# } # update.transparency()

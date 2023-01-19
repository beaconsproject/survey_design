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
    if (input$inv == 'forest_pct') {
      pal <- 'YlGn'
    } else if (input$inv %in% c('water_pct','wetland_pct')) {
      pal <- 'PuBu'
    } else if (input$inv %in% c('quartz_pct','placer_pct')) {
      pal <- 'Greys'
    } else if (input$inv %in% c('elev_median','elev_min','elev_max','elev_sd')) {
      pal <- '-BrBG'
    } else {
      pal <- 'Reds'
    }
    
    
    # If the user clicks the button to generate clusters
    if (input$clustButton) {
      # print('if(input$clustButton)')
      a.pal <- c('#7fc97f','#beaed4','#fdc086','#ffff99','#386cb0','#f0027f','#bf5b17','#666666')
      m <- tm_shape(data$clusters) + tm_fill('clusters', palette = a.pal,
                                             alpha = 0.65,
                                             group = 'Clusters',
                                             # zindex determines the order that elements
                                             # appear on the page; an element with a
                                             # higher z index appears in front of a lower
                                             # zindex
                                             #zindex = 500
      )
    } else {
      # print('in else')
      m <- tm_shape(data$factors) + tm_fill(input$inv, palette = pal, 
                                            style = input$style,
                                            alpha = 1,
                                            colorNA = NULL, 
                                            group = input$inv,
                                            title = input$inv)
    }
    
    # Base map
    m <- m + tm_shape(data$grid) + tm_borders(group='Grid') +
      tm_shape(data$linear) + tm_lines(col = 'black',
                                       alpha = 1,
                                       group='Linear features') +
      tm_shape(data$areal) + tm_fill(col = 'red',
                                     alpha = 1,
                                     group='Areal features') +
      tm_shape(data$thtt) + tm_borders(col = 'black',
                                       alpha = 1,
                                       group='TH traditional territory') +
      tm_shape(data$settlement) + tm_fill(col = 'blue', 
                                          alpha = .5,
                                          group='Settlement lands') + 
      tm_shape(data$study_boundary) + tm_borders(col='black', lwd=2, group='Study boundary')
    
    m <- m + tm_legend(position = c('right', 'top'), frame = T)
    # print('reached end of tmap rendering')
    m
    lf <- tmap_leaflet(m)
    lf %>% leaflet::hideGroup(c("Areal disturbances","Linear disturbances",
                                "Areal features","Linear features","Elevation",
                                "Stratified random","TH traditional territory",
                                "Settlement lands","Camera traps (stratified)"))
  }) # renderTmap
  print('finished render.map1')
} # render.map1

map.selected.cells <- function(input, output, session, data) {
  # add randomly selected cells
  print('in map.selected.cells')
  
  tmapProxy('map1', session, x = {
    n1rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n1), 3, strata='id'))
    n2rnd3 <- st_as_sf(terra::spatSample(terra::vect(data$n2), 3, strata='id'))
    tm_shape(data$n2) + tm_borders(col = 'blue', lwd = 2,
                                   group = 'Stratified random') +
      tm_shape(n2rnd3) + tm_dots(size=0.02, col='blue', group="Camera traps (stratified)") +
      tm_shape(data$n1) + tm_borders(col = 'black', lwd = 2,
                                     group = 'Simple random') +
      tm_shape(n1rnd3) + tm_dots(size=0.02, col='black', group="Camera traps (simple)")
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
    
    tmapProxy('map1',
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

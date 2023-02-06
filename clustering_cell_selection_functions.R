# create reactive clusters() object (data$clusters)
# 

create.clusters <- function(input, session, data) {
  # value to split between low/medium and high disturbance
  split <- 30
  
  ## Change NAs to 0
  x <- data$factors %>%
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
  
  ## Split factors into 1) merge100_pct == 0, 2) 0 < merge100_pct <= 30, 
  ## 3) merge100_pct > 30
  
  y <- x %>% 
    mutate(clusters = ifelse(test = merge100_pct == 0, yes = 1,
                            no = ifelse(test = merge100_pct > 0 & 
                                          merge100_pct <= split, 
                                        yes = 2, no = 3)))
  
  ## Split factors into 1) merge100_pct == 0, 2) 0 < merge100_pct <= 30, 
  ## 3) merge100_pct > 30
  data$clusters <- y 
  return(data)
}
 
# create.clusters <- function(input, session, data) {
#   # print('in create.clusters')
#   # shinybusy::add_busy_spinner()
#   # duplicate data$factors, replace NAs with 0s 
#   x <- data$factors %>%
#     mutate(
#       placer_pct=ifelse(is.na(placer_pct),0,placer_pct),
#       quartz_pct=ifelse(is.na(quartz_pct),0,quartz_pct),
#       recent_fires_pct=ifelse(is.na(recent_fires_pct),0,recent_fires_pct),
#       area500_pct=ifelse(is.na(area500_pct),0,area500_pct),
#       line500_pct=ifelse(is.na(line500_pct),0,line500_pct),
#       merge100_pct=ifelse(is.na(merge100_pct),0,merge100_pct),
#       water_pct=ifelse(is.na(water_pct),0,water_pct),
#       forest_pct=ifelse(is.na(forest_pct),0,forest_pct),
#       wetland_pct=ifelse(is.na(wetland_pct),0,wetland_pct)
#      )
#   
#   # select only factors that user wants to cluster by
#   # st_drop_geometry() drops geom field from table (geom describes what type of
#   # feature it is and has some numbers describing it)
#   # print('intersecting study boundary and factors')
#   y <- select(x, unlist(input$factors), id, grid_m2) %>%
#     #st_intersection(data$study_boundary) %>%
#     #filter(grid_m2 > 23000000) %>%
#     st_drop_geometry()
# 
#   # Cluster
#   # kmeans() returns a list; $cluster object is a vector where the name is the
#   # cell number and the value is what cluster it's in
#   if (input$zero) {
#   # pull out undisturbed cells and cluster
#     if ('merge100_pct' %in% input$factors) {
#       undisturbed <- y %>%
#         filter(merge100_pct == 0)
#       y <- y %>%
#         filter(merge100_pct > 0)
#      
#       clust_undisturbed <- rep(0, nrow(undisturbed))
#     }
#   }
# 
#   # cluster
#   clust <- kmeans(scale(y), input$clusters)$cluster
#  
#   # View(clust)
#   # print('kmeans')
#   
#   # add field to x with what cluster the cell is in
#   x2 <- x %>%
#     filter(id %in% y$id) %>%
#     mutate(clusters = clust)
# 
#   # first we cluster, then we replace clusters with 0 if merge100_pct==0
#   #if (input$zero) {
#   #  x <- mutate(x, clusters=ifelse(merge100_pct==0, 0, clusters))
#   #}
#   
#   if (input$zero) {
#     if ('merge100_pct' %in% input$factors) {
#       x_undisturbed <- x %>%
#         filter(id %in% undisturbed$id) %>%
#         mutate(clusters = clust_undisturbed)
#       
#       x2 <- rbind(x2, x_undisturbed)
#     }
#   }
# 
#   # print('mutate(clusters = clust)')
#   
#   # shinybusy::remove_modal_spinner()
#   # print(x)
#   # print(class(x))
#   data$clusters <- x2
#   # print('finished create.clusters')
#   return(data)
# }

create.dta1 <- function(data) {
  print('in create.dta1')
  # View(data$n2)
  # print(data$clusters)
  # print(is.null(data$clusters))
  
  if (is.null(data$clusters)) {
  # if (is.null(data$n2)) {
    print('returning early')
    return()
  }
  
  data$dta1 <- data$clusters %>%
  # data$dta1 <- data$n3
    st_drop_geometry() %>%
    group_by(clusters) %>%
    summarize(n = n(),
              merge100_pct = round(mean(merge100_pct),1),
              recent_fires_pct = round(mean(recent_fires_pct),1),
              #quartz_pct = round(mean(quartz_pct),1),
              #placer_pct = round(mean(placer_pct),1),
              elev_median = round(mean(elev_median),1),
              elev_sd = round(mean(elev_sd),1),
              forest_pct = round(mean(forest_pct),1),
              water_pct = round(mean(water_pct),1),
              wetland_pct = round(mean(wetland_pct),1)
             )
  print('finished create.dta1')
  return(data)
} # create.dta1()

render.tab1 <- function(output, data) {
  print('in render.tab1')
  output$tab1 <- DT::renderDataTable({
    x <- data$dta1
    datatable(x, rownames = F, 
              options = list(dom = 'tip', 
                             scrollX = T, 
                             scrollY = T,
                             pageLength = 25),
              class = 'compact')
  })
  print('finished render.tab1')
}

render.tab2 <- function(output, data) {
  output$tab2 <- DT::renderDataTable({
 
    ks_output_simple <-
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$n1$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$n1$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$n1$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$n1$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$n1$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$n1$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$n1$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$n1$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$n1$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$n1$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$n1$water_pct)[[1]]
      )
    
    ks_output_stratified <-
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$n3$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$n3$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$n3$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$n3$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$n3$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$n3$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$n3$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$n3$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$n3$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$n3$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$n3$water_pct)[[1]]
      )
    
    ks_output <-
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$simple$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$simple$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$simple$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$simple$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$simple$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$simple$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$simple$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$simple$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$simple$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$simple$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$simple$water_pct)[[1]]
      )
    
    #  filter(cluster %in% c(2,3))
    #data$simple13 <- data$simple %>% 
    #  filter(cluster %in% c(1,3))
    
    ks_output_clust1 <- 
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$simple1$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$simple1$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$simple1$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$simple1$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$simple1$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$simple1$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$simple1$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$simple1$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$simple1$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$simple1$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$simple1$water_pct)[[1]]
      )
    
    ks_output_clust2 <- 
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$simple2$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$simple2$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$simple2$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$simple2$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$simple2$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$simple2$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$simple2$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$simple2$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$simple2$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$simple2$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$simple2$water_pct)[[1]]
      )
    
    ks_output_clust3 <- 
      tibble(
        merge100_pct = ks.test(data$factors$merge100_pct, data$simple3$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$factors$merge500_pct, data$simple3$merge500_pct)[[1]],
        placer_pct = ks.test(data$factors$placer_pct, data$simple3$placer_pct)[[1]],
        quartz_pct = ks.test(data$factors$quartz_pct, data$simple3$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$factors$recent_fires_pct, data$simple3$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$factors$benchmark_pct, data$simple3$benchmark_pct)[[1]],
        elev_median = ks.test(data$factors$elev_median, data$simple3$elev_median)[[1]],
        elev_sd = ks.test(data$factors$elev_sd, data$simple3$elev_sd)[[1]],
        forest_pct = ks.test(data$factors$forest_pct, data$simple3$forest_pct)[[1]],
        wetland_pct = ks.test(data$factors$wetland_pct, data$simple3$wetland_pct)[[1]],
        water_pct = ks.test(data$factors$water_pct, data$simple3$water_pct)[[1]]
      )
    
    ks_output_clust1vs2 <- 
      tibble(
        merge100_pct = ks.test(data$simple1$merge100_pct, data$simple2$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$simple1$merge500_pct, data$simple2$merge500_pct)[[1]],
        placer_pct = ks.test(data$simple1$placer_pct, data$simple2$placer_pct)[[1]],
        quartz_pct = ks.test(data$simple1$quartz_pct, data$simple2$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$simple1$recent_fires_pct, data$simple2$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$simple1$benchmark_pct, data$simple2$benchmark_pct)[[1]],
        elev_median = ks.test(data$simple1$elev_median, data$simple2$elev_median)[[1]],
        elev_sd = ks.test(data$simple1$elev_sd, data$simple2$elev_sd)[[1]],
        forest_pct = ks.test(data$simple1$forest_pct, data$simple2$forest_pct)[[1]],
        wetland_pct = ks.test(data$simple1$wetland_pct, data$simple2$wetland_pct)[[1]],
        water_pct = ks.test(data$simple1$water_pct, data$simple2$water_pct)[[1]]
      )
    
    ks_output_clust1vs3 <- 
      tibble(
        merge100_pct = ks.test(data$simple1$merge100_pct, data$simple3$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$simple1$merge500_pct, data$simple2$merge500_pct)[[1]],
        placer_pct = ks.test(data$simple1$placer_pct, data$simple3$placer_pct)[[1]],
        quartz_pct = ks.test(data$simple1$quartz_pct, data$simple3$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$simple1$recent_fires_pct, data$simple3$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$simple1$benchmark_pct, data$simple3$benchmark_pct)[[1]],
        elev_median = ks.test(data$simple1$elev_median, data$simple3$elev_median)[[1]],
        elev_sd = ks.test(data$simple1$elev_sd, data$simple3$elev_sd)[[1]],
        forest_pct = ks.test(data$simple1$forest_pct, data$simple3$forest_pct)[[1]],
        wetland_pct = ks.test(data$simple1$wetland_pct, data$simple3$wetland_pct)[[1]],
        water_pct = ks.test(data$simple1$water_pct, data$simple3$water_pct)[[1]]
      )
    
    ks_output_clust2vs3 <- 
      tibble(
        merge100_pct = ks.test(data$simple2$merge100_pct, data$simple3$merge100_pct)[[1]],
        #merge500_pct = ks.test(data$simple2$merge500_pct, data$simple2$merge500_pct)[[1]],
        placer_pct = ks.test(data$simple2$placer_pct, data$simple3$placer_pct)[[1]],
        quartz_pct = ks.test(data$simple2$quartz_pct, data$simple3$quartz_pct)[[1]],
        recent_fires_pct = ks.test(data$simple2$recent_fires_pct, data$simple3$recent_fires_pct)[[1]],
        benchmark_pct = ks.test(data$simple2$benchmark_pct, data$simple3$benchmark_pct)[[1]],
        elev_median = ks.test(data$simple2$elev_median, data$simple3$elev_median)[[1]],
        elev_sd = ks.test(data$simple2$elev_sd, data$simple3$elev_sd)[[1]],
        forest_pct = ks.test(data$simple2$forest_pct, data$simple3$forest_pct)[[1]],
        wetland_pct = ks.test(data$simple2$wetland_pct, data$simple3$wetland_pct)[[1]],
        water_pct = ks.test(data$simple2$water_pct, data$simple3$water_pct)[[1]]
      )
    
    labels <- tibble(ks_test=c('Simple_Random vs All_cells', 'Stratified_Random vs All_cells', 'Full_sample vs All_cells', 'Clust1_sample vs All_cells', 
                               'Clust2_sample vs All_cells', 'Clust3_sample vs All_cells', 
                               'Clust1_sample vs Clust2 sample', 'Clust1_sample vs Clust3_sample', 'Clust2_sample vs Clust3_sample'))
    ks_tests <- bind_rows(ks_output_simple, ks_output_stratified, ks_output, ks_output_clust1, ks_output_clust2, ks_output_clust3,
                          ks_output_clust1vs2, ks_output_clust1vs3, ks_output_clust2vs3)
    ks_tests <- bind_cols(labels, ks_tests)
    
    datatable(ks_tests, rownames = F, options = list(dom = 'tip', scrollX = T,
                                                           scrollY = T, pageLength = 25),
                          class = 'compact') %>%
                  formatRound(columns = c('merge100_pct', 'placer_pct', 'quartz_pct',
                                          'recent_fires_pct', 'benchmark_pct', 'elev_median',
                                          'elev_sd', 'forest_pct', 'wetland_pct', 'water_pct'),
                              digits = 3)
  })
}

# render.tab2 <- function(output, data) {
#   print('in render.tab2')
#   output$tab2 <- DT::renderDataTable({
#     print('in renderDataTable for tab2')
#     simple <- tibble(type = 'simple', id = pull(data$n1, id)) %>%
#       left_join(data$factors)
#     
#     stratified <- tibble(type = 'stratified', id = pull(data$n2, id)) %>%
#       left_join(data$factors)
#     
#     ks_simple <- 
#       tibble(type = 'simple',
#              merge100_pct = ks.test(data$factors$merge100_pct, simple$merge100_pct)[[1]],
#              #merge500_pct = ks.test(data$factors$merge500_pct, simple$merge500_pct)[[1]],
#              placer_pct = ks.test(data$factors$placer_pct, simple$placer_pct)[[1]],
#              quartz_pct = ks.test(data$factors$quartz_pct, simple$quartz_pct)[[1]],
#              recent_fires_pct = ks.test(data$factors$recent_fires_pct, simple$recent_fires_pct)[[1]],
#              benchmark_pct = ks.test(data$factors$benchmark_pct, simple$benchmark_pct)[[1]],
#              elev_median = ks.test(data$factors$elev_median, simple$elev_median)[[1]],
#              elev_sd = ks.test(data$factors$elev_sd, simple$elev_sd)[[1]],
#              forest_pct = ks.test(data$factors$forest_pct, simple$forest_pct)[[1]],
#              wetland_pct = ks.test(data$factors$wetland_pct, simple$wetland_pct)[[1]],
#              water_pct = ks.test(data$factors$water_pct, simple$water_pct)[[1]]
#       )
#     
#     ks_stratified <- 
#       tibble(type="stratified",
#              merge100_pct = ks.test(data$factors$merge100_pct, stratified$merge100_pct)[[1]],
#              #merge500_pct = ks.test(data$factors$merge500_pct, stratified$merge500_pct)[[1]],
#              placer_pct = ks.test(data$factors$placer_pct, stratified$placer_pct)[[1]],
#              quartz_pct = ks.test(data$factors$quartz_pct, stratified$quartz_pct)[[1]],
#              recent_fires_pct = ks.test(data$factors$recent_fires_pct, stratified$recent_fires_pct)[[1]],
#              benchmark_pct = ks.test(data$factors$benchmark_pct, stratified$benchmark_pct)[[1]],
#              elev_median = ks.test(data$factors$elev_median, stratified$elev_median)[[1]],
#              elev_sd = ks.test(data$factors$elev_sd, stratified$elev_sd)[[1]],
#              forest_pct = ks.test(data$factors$forest_pct, stratified$forest_pct)[[1]],
#              wetland_pct = ks.test(data$factors$wetland_pct, stratified$wetland_pct)[[1]],
#              water_pct = ks.test(data$factors$water_pct, stratified$water_pct)[[1]])
#     # View(ks_simple)
#     # View(ks_stratified)
#     
#     ks <- bind_rows(ks_simple, ks_stratified)
# 
#     print('finished renderDataTable for tab2')
#     
#     datatable(ks, rownames = F, options = list(dom = 'tip', scrollX = T,
#                                                scrollY = T, pageLength = 25),
#               class = 'compact') %>%
#       formatRound(columns = c('merge100_pct', 'placer_pct', 'quartz_pct', 
#                               'recent_fires_pct', 'benchmark_pct', 'elev_median',
#                               'elev_sd', 'forest_pct', 'wetland_pct', 'water_pct'),
#                   digits = 3)
#   }) # renderDataTable
#   print('finished render.tab2')
# } # render.tab2()


sample <- function(input, data) {
  ## Simple Random
  data$n1 <- sample_n(data$clusters, input$size * as.numeric(input$clusters))
  
  ####
  
  ## Stratified Random 
  # number of cells to sample from each disturbance group
  n.sample.1 <- 40
  n.sample.2 <- 60
  n.sample.3 <- 40
  
  # sample 40 from cluster 1, 60 from cluster 2, and 40 from cluster 3
  # 
  
  data$simple3 <- data$clusters %>% 
  filter(clusters == 3) %>%
    slice_sample(n = n.sample.3)
  
  elev.mean <- mean(data$simple3$elev_median)
  elev.sd <- sd(data$simple3$elev_median)
  
  wetland.median <- mean(data$simple3$wetland_pct)
  wetland.sd <- sd(data$simple3$wetland_pct)
  
  
  
  data$simple1 <- data$clusters %>%
    dplyr::filter(clusters == 1) %>%
    dplyr::filter(
           elev_median > (elev.mean - elev.sd),
           elev_median < (elev.mean + elev.sd),
           wetland_pct > (wetland.median - elev.sd),
           wetland_pct < (wetland.median + wetland.sd)
           ) %>%
    slice_sample(n = 60)
  
  
  data$simple2 <- data$clusters %>%
    dplyr::filter(clusters == 2) %>%
    dplyr::filter(
           elev_median > (elev.mean - elev.sd),
           elev_median < (elev.mean + elev.sd),
           wetland_pct > (wetland.median - elev.sd),
           wetland_pct < (wetland.median + wetland.sd)
           ) %>%
    slice_sample(n = 60)
  

  
  data$simple <- rbind(data$simple1, data$simple2, data$simple3)
  # data$simple_all_cells <- 
  data$n2 <- data$simple
  
  # n3 is stratified random sampling; ie same number sampled from each cluster
  data$n3 <- data$clusters %>%
    group_by(clusters) %>%
    sample_n(size = input$size)
  print('sampled successfully')
  #return(data)

  # n4 is simple random sample using 140 cells
  data$n1 <- data$factors %>%
    sample_n(size = 140)
  #print('sampled successfully')
  return(data)
  #data$n1 <- data$simple

}

# strat.sample <- function(input, data) {
#   print('in strat sample')
#   ####
#   # n1 is random sampling; ie same number is not sampled from each cluster
#   # sample from data$clusters (defined above), number to be samples is number of
#   #   samples per cluster * number of clusters
#   if (input$thlands==TRUE) {
#     # force TH settlement land grids
#     # cells that are primarily
#     th <- filter(data$factors, settlements_pct > input$thlands_pct)
#     not_th <- filter(data$factors, !id %in% th$id) # grids with <x% settlement lands
#     nsize <- (input$size*as.numeric(input$clusters)) - nrow(th) # number of remaining cells to sample from
#     
#     clusters_th <- filter(data$clusters, id %in% th$id)
#     clusters_not_th <- filter(data$clusters, !id %in% th$id)
#     
#     # check to see if there are enough cells with settlement land to only use 
#     # those for deployments
#     if (nsize<=0) {
#       # if so, only grab settlement land cells from cluster
#       data$n1 <- clusters_th
#     } else {
#       # if not, randomly select the required number from non-settlement cells
#       nrnd <- sample_n(not_th, nsize)
#       # combine them with the settlement cells
#       total <- bind_rows(th, nrnd)
#       data$n1 <- filter(data$clusters, id %in% total$id)
#     }
#   } else {
#     data$n1 <- sample_n(data$clusters, input$size * as.numeric(input$clusters))
#   }
#   
#   ####
  # # n2 is stratified random sampling; ie same number sampled from each cluster
  # 
  # if (input$thlands) {
  #   print('prioritizing th lands')
  # 
  #   strat.random <- data.frame(matrix(nrow = 0, ncol = ncol(data$clusters)))
  #   colnames(strat.random) = colnames(data$clusters)
  # 
  #   for (i in 1:input$clusters) {
  #     tmp.th <- filter(clusters_th, clusters %in% i)
  #     # fewer cells with settlement land than cells that need to be selected
  #     if (nrow(tmp.th) < input$size) {
  #       tmp.not.th <- sample_n(clusters_not_th, input$size - nrow(tmp))
  #       tmp <- rbind(tmp.th, tmp.not.th)
  #     } else {
  #       tmp <- sample_n(tmp.th, input$size)
  #     }
  # 
  #     strat.random <- rbind(strat.random, tmp)
  # 
  #   }
  # 
  #   data$n2 <- strat.random
  # 
  # } else {
  # 
  #   data$n2 <- data$clusters %>%
  #     group_by(clusters) %>%
  #     sample_n(size = input$size)
#     
#   }
#   print('finished strat.sample')
#   
#   return(data)
# }




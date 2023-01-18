## Load Required Libraries
library(shiny)
library(sf)
library(sp)
library(DT)
library(tmap)
library(dplyr)
library(magrittr)
library(leaflet)
library(tidyverse)
library(shinydashboard)
library(shinybusy)

functionspath <- list.files(pattern = 'functions.R', full.names = T)
source(functionspath)

# clicklist <- list()

## Set up map options
tmap_mode('view')
tmap_options(check.and.fix = T)
# Basemap 
tmap_options(basemaps = c("Esri.WorldTopoMap","Esri.NatGeoWorldMap","Esri.WorldImagery"))
#tm_basemap(leaflet::providers$Esri.WorldImagery)

shinybusy::use_busy_spinner()

ui = dashboardPage(
  dashboardHeader(title = 'Wolverines Survey'),
  dashboardSidebar(
    
    sidebarMenu(
      menuItem('Sampling Design', tabName = 'fri', icon = icon('th'))
    ), # sidebarMenu
    
    #### Survey Factors 
    ####
    
    h5('(1) View covariates'),
    # Select feature type (merge_100, percent_forest, etc) to view
    selectInput("inv", label = "Select feature:", 
                # choices = names(factors)[3:22],
                # selected = 'merge_100'),
                # choices defined server side 
                choices = NULL,
                selected = NULL),
    # Style for binning cells for display
    selectInput("style", label="Select style:", 
                choices=c("jenks","kmeans","quantile","pretty","equal"), 
                selected="quantile"), 
    # Set transparency
    #sliderInput("alpha", label="Transparency:", min=0, max=1, value=1, step=0.1, 
    #            ticks=FALSE),
    hr(),
    h5('(2) Generate clusters'),
    # features to cluster by
    selectInput("factors", label = "Select features:", multiple=TRUE,
                # choices = names(factors)[3:22],
                # selected=c('merge100_pct','elev_median','elev_sd','forest_pct',
                #            'water_pct')
                choices = NULL,
                selected = NULL
               ),
    
    #checkboxInput('th.settlement', label = 'Show TH Settlement Lands',
    #              value = T
    #             ),
    
    # actionButton('redraw', label = 'Redraw study boundaries')
    
    #### Clusters 
    ####
    # how many clusters
    sliderInput("clusters", label="Number of clusters:", min=0, max=20, value=4, 
                ticks=FALSE),
    # button to generate clusters
    actionButton("clustButton", "Generate clusters"),
    hr(),
    
    #### Site Selection
    #### 
    
    h5('(3) Select grids'),
    # select random sites
    # slider for how many cells to select from each bin
    sliderInput("size", label="Sample size per strata:", min=0, max=100, 
                value=25, step=5, ticks=FALSE),
    # button to select random sites
    actionButton("goButton", "Select random grids")

  ), # dashboardSidebar
  
  dashboardBody(
    tabItems(
      tabItem(tabName = 'fri',
              fluidRow(
                tabBox(
                  id = 'one', width = '12',
                  tabPanel('Mapview', leafletOutput('map1', height = 900)),
                  tabPanel('Clusters', DT::dataTableOutput('tab1')),
                  tabPanel('Similarity', DT::dataTableOutput('tab2'))
                ) # tabBox
              ) # fluidRow
      ) # tabItem
    ) # tabItems
  ) # dashboardBody
) # ui

server <- function(input, output, session) {
  
  # load factors, linear, areal, and grid into a single reactiveValues object; it
  #   is essentially a list that you can pass into function as a single argument
  data <- load.data()
  
  observe({
    # This populates the dropdown fields that are dependent on factors (now 
    # data$factors)
    update.inputs(input, session, data)
    # print('inputs updated')
  })

  # creates the clusters once the user hits the make clusters button
  observeEvent(input$clustButton, {
    print('clustButton clicked')
    data <- create.clusters(input, session, data)
  })

  # needs to be wrapped in observe() since data is a reactive object
  observe({
    data <- create.dta1(data)
  })

  # ditto about observe()
  observe({
    render.map1(input, output, session, data)
  })
  
  observe({
    update.transparency(input, session, data)
  })
  
  observe({
    render.tab1(output, data)  
  })
  
  observeEvent(input$goButton, {
    data <- strat.sample(input, data)
    map.selected.cells(input, output, session, data)
    render.tab2(output, session, data)
  })
  
  observeEvent(input$map1_click, {
    data <- modify.study.boundary(input, output, session, data)
  })
}
shinyApp(ui, server)








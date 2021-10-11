# setup                 ---------------------------------------------------------------

library(dplyr)
library(geosphere)
library(leaflet)
library(readr)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(shiny.semantic)
library(tidyverse)

options(spinner.color = "#2c3e50")

# dropdown module
dropdown = function(id, value, choices, label) {
  ns <- NS(id)
  tagList(
    h5(style = 'color:#2c3e50', label),
    dropdown_input(
      input_id = ns('dropdown'),
      value = value,
      choices = choices
    ),
    br()
  )
}

# distance function
distance_f = function(data,
                    vessel,
                    exclude_parked = T,
                    toggle_port = T,
                    port = NULL){
  
  data = data %>%
    # filter vessel
    filter(SHIPNAME == vessel)
  
  # filter out parked
  if (exclude_parked) {
    data = data %>%
      filter(is_parked == 0)
  }
  
  # consider port filter
  if(toggle_port & !is.null(port)) {
    data = data %>%
      filter(PORT == !!port)
  }
  
  # check length after filtering
  if(nrow(data)==0){
    return(list(dt_max = NULL,
                dt_not_max = NULL))
  }
  
  data = data %>%
    # calculate distance
    mutate(DISTANCE = distHaversine(cbind(LON, LAT),
                                    cbind(lead(LON), lead(LAT)))) %>%
    # identify maximum distance
    mutate(max = max(DISTANCE, na.rm = T)) %>%
    mutate(is_max = if_else(DISTANCE == max, T, F)) %>%
    mutate(is_max = is_max | lag(is_max)) %>%
    select(LAT, LON, SHIPNAME, DATETIME, is_max, DISTANCE)
  
  
  # extract max distance points
  data_max = data %>%
    filter(is_max)
  
  # if 2 identical max distances
  if (nrow(data_max) > 2) {
    group = rep(seq(1, nrow(data_max) / 2), each = 2)
    data_max$group = group
    data_max = data_max %>%
      group_by(group) %>%
      mutate(mean_date = mean(DATETIME)) %>%
      ungroup() %>%
      arrange(-mean_date) %>%
      select(-mean_date)
    data_max = dt_max[1:2,]
  }
  
  # filter out max distance
  data_not_max = data %>%
    filter(!is_max)
  
  return(list(dt_max = data_max,
              dt_not_max = data_not_max))
}
# map function
map_f = function(markers = NULL,
                 dt_max = NULL,
                 dt_not_max = NULL){
  # generate map base
  m = leaflet() %>%
    addProviderTiles(providers$Stamen.Watercolor, options = list(opacity = .3)) %>%
    addProviderTiles(providers$Stamen.TonerLite) %>%
    addProviderTiles(providers$Stamen.TonerLabels)
  
  # check minimum inputs
  if(is.null(dt_max)){
    return(m)
  }
  if(is.null(markers)){
    markers = 'lines'
  }
  
  # add markers/lines
  if (markers == 'lines') {
    m = m %>%
      addPolylines(
        data = dt_not_max,
        lat = ~ LAT,
        lng = ~ LON,
        fillOpacity = 1,
        color = 'grey'
      ) %>%
      addPolylines(
        data = dt_max,
        lat = ~ LAT,
        lng = ~ LON,
        fillOpacity = 1,
        color = 'red'
      ) %>%
      addCircleMarkers(
        data = dt_max,
        stroke = T,
        color = 'red',
        fillColor = 'pink',
        lat = ~ LAT,
        lng = ~ LON,
        fillOpacity = 1
      )
  }
  else if (markers == 'circles') {
    m = m %>%
      addCircleMarkers(
        data = dt_not_max,
        stroke = F,
        fillColor = 'grey',
        lat = ~ LAT,
        lng = ~ LON,
        fillOpacity = 0.1
      ) %>%
      addCircleMarkers(
        data = dt_max,
        stroke = T,
        color = 'red',
        fillColor = 'pink',
        lat = ~ LAT,
        lng = ~ LON,
        fillOpacity = 1
      )
  }
  return(m)
}

# ui                    ----------------------------------------------------------------------
ui <- semanticPage(
  # head                --------------------------------------------------------------------
  
  
  theme = 'solar',
  suppress_bootstrap = F,
  tags$head(
    HTML('<link rel="shortcut icon" href="img/favicon.ico"/>'),
    HTML(
      '<link type="text/css" rel="stylesheet" href="css/styles.css"/>'
    )
  ),
  title = 'Ship Map',
  h1('Ship Map'),
  sidebar_layout(
    # sidebar           -----------------------------------------------------------------
    sidebar_panel = sidebar_panel(
      h3('Vessel Settings'),
      withSpinner(uiOutput('dropdown_vessel_type')),
      uiOutput('dropdown_vessel'),
      uiOutput('dropdown_port'),
      accordion(
        active_title = "Additional Settings",
        accordion_list = list(
          list(
            title = "Additional Settings",
            content = list(uiOutput('dropdown_markers'),
                           uiOutput('toggles'))
          )
      ))
    ),
    
    # main              --------------------------------------------------------------------
    main_panel = main_panel(
      h3('Map'),
      withSpinner(leafletOutput('map', height = '400px'),),
      hr(),
      uiOutput('note')
    )
  )
)

# server                ------------------------------------------------------------------
server <- function(input, output, session) {
  # data                ####
  get_data = reactive({
    data = read_csv('data/ships.csv')
  })
  get_vessel_types = reactive({
    req(get_data())
    
    get_data() %>%
      pull(ship_type) %>%
      unique()
  })
  get_vessel = reactive({
    req(get_data(),input$`vessel_type-dropdown`)
    
    get_data() %>%
      filter(ship_type == input$`vessel_type-dropdown`) %>%
      pull(SHIPNAME) %>%
      unique() 
  })
  get_port = reactive({
    req(get_data(), input$`vessel-dropdown`)
    
    get_data() %>%
      filter(SHIPNAME == input$`vessel-dropdown`) %>%
      pull(PORT) %>%
      unique()
    
  })
  
  # distance            ####
  get_distance = reactive({
    req(get_data(),
        input$`vessel-dropdown`,
        input$`port-dropdown`)
    
    distance_f(
      data = get_data(),
      vessel = input$`vessel-dropdown`,
      port = input$`port-dropdown`,
      exclude_parked = input$exclude_parked,
      toggle_port = input$toggle_port
    )
    
  })

  # vessel settings     ####  
  output$dropdown_vessel_type = renderUI({
    req(get_vessel_types())
    dropdown(
      id = "vessel_type",
      value = get_vessel_types()[1],
      choices = get_vessel_types(),
      label = 'Vessel Type'
    )
    
  })
  output$dropdown_vessel = renderUI({
    req(get_vessel())
    dropdown(
      id = "vessel",
      value = get_vessel()[1],
      choices = get_vessel(),
      label = 'Vessel Name'
    )
    
  })
  output$dropdown_port = renderUI({
    req(get_port())
    dropdown(
      id = "port",
      value = get_port()[1],
      choices = get_port(),
      label = 'Port'
    )
  })
  
  # additional settings ####  
  output$dropdown_markers = renderUI({
    dropdown(
      id = "markers",
      value = 'lines',
      choices = c('lines', 'circles'),
      label = 'Marker Type'
    )
  })
  output$toggles = renderUI({
    tagList(
      h5(style = 'color:#2c3e50', 'Switches'),
      toggle("exclude_parked", "Exclude Parked", T, style = 'margin:10px'),
      toggle("toggle_port", "Use Port", T, style = 'margin:10px')
    )
  })
  
  # map                 ####
  output$map = renderLeaflet({
    req(get_distance())
    
    markers = input$`markers-dropdown`
    dt_max = get_distance()$dt_max
    dt_not_max = get_distance()$dt_not_max
    
    map_f(markers,dt_max,dt_not_max)
  })
  output$note = renderUI({
    req(get_distance()$dt_max$DISTANCE[1])
    tagList(strong(paste0(
      'Distance: ',
      round(get_distance()$dt_max$DISTANCE[1], 0),
      ' meters'
    )))
  })
  
}

shinyApp(ui, server)
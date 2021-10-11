library(dplyr)
library(leaflet)
library(readr)
library(testthat)
library(tidyverse)

# data = read_csv('data/ships.csv')
# vessel = 'VTS HEL'
# vessel = 'KAROLI'
# port = 'gdansk'
# exclude_parked = F
# exclude_parked = T
# toggle_port = F
# toggle_port = T

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


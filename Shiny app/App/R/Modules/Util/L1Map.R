L1Map_UI <- function(id) {
  ns <- NS(id)
  tagList(
    leafletOutput(ns('map'), height = '500px')
  )
}

L1Map_Server <- function(id,data,dataFiltered, citySelected, options){
  moduleServer(id,function(input, output, session) {
    
    ## Base Proxy Map
    output$map <- renderLeaflet({
      l1_lat_long = data %>% select(salid1, lat, long) %>% distinct()
      leaflet(l1_lat_long,options = leafletOptions(zoomControl = FALSE)) %>% 
        addProviderTiles("Esri.WorldGrayCanvas") %>%
        # fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
        setView(lng = mean(l1_lat_long$long), lat = mean(l1_lat_long$lat) , zoom=3)
    })
    
    ## Reactive expression for color pallete, changes as data changes
    colorpal <- reactive({
      validate( need(nrow(dataFiltered())>0,"Need data") )
      metricTmp = unique(dataFiltered()$metric)
      colorNumeric("plasma", dataFiltered()$value,reverse = !grepl("Temperature",metricTmp)) 
    })
    
    ## Update leaflet based on data() 
    observe({
      validate(  need(nrow(dataFiltered())>0,"Need data"))
      zoomTmp = input$map_zoom
      print(zoomTmp)
      radiusDefault = 50000
      radiusTmp = case_when(
        zoomTmp<5~radiusDefault,
        zoomTmp==5~radiusDefault*0.7,
        zoomTmp==6~radiusDefault*0.5,
        zoomTmp==7~radiusDefault*0.3,
        TRUE~radiusDefault*0.2)
      if (!all(is.na(dataFiltered()$hex))){
        dataFiltered = dataFiltered()
        labelsTmp = options$leaflet_legend_labels
        if (unique(dataFiltered$metric)!='Mortality risk per 1C lower extreme cold'){labelsTmp = sort(unique(dataFiltered$cat))}
        leafletProxy("map", data = dataFiltered) %>%
          clearShapes()  %>% 
          clearControls() %>%
          addCircles(radius = radiusTmp, weight = 1, color = "#777777",
                     fillColor = ~hex, fillOpacity = 0.9 , 
                     layerId = ~salid1,
                     label = ~tooltip__map %>% map(~HTML(.x))
          ) %>% 
          addLegend(position = "bottomright",
                    title = str_wrap_leaflet_legend_title(unique(dataFiltered()$metric)),
                    opacity = 0.9,
                    colors = options$leaflet_legend_colors, 
                    labels  =  labelsTmp  )
      } else {
        pal <- colorpal()
        leafletProxy("map", data = dataFiltered()) %>%
          clearShapes()  %>% 
          clearControls() %>%
          addCircles(radius = radiusTmp, weight = 1, color = "#777777",
                     fillColor = ~pal(value), fillOpacity = 0.9 , 
                     layerId = ~salid1,
                     label = ~tooltip__map %>% map(~HTML(.x))
          ) %>% 
          addLegend(position = "bottomright",
                    title = str_wrap_leaflet_legend_title(unique(dataFiltered()$metric)),
                    opacity = 0.9,
                    pal = pal, values = ~value)
      }
      
    })
    
    ## Update leaflet based on citySelected()
    observe({
      if (!is.null(citySelected)){
        ## Get centroid
        req(citySelected())
        ptTmp = dataFiltered() %>% filter(salid1 == citySelected()) 
        longTmp = as.numeric( ptTmp$long)
        latTmp = as.numeric(ptTmp$lat)
        
        ## Popup content
        popupContent =HTML(ptTmp$tooltip__map)
        
        ## Manipulate map
        leafletProxy("map") %>% 
          clearPopups() %>% 
          setView(lng = longTmp,lat=latTmp, zoom =5) %>%
          addPopups(longTmp, latTmp, popupContent )
      }
    })
    
    ## Handles map clicks
    observe({
      event <- input$map_shape_click
      leafletProxy("map") %>% clearPopups()
    })
    
    ## Returns SALID clicked 
    reactive({
      event <- input$map_shape_click
      event$id
    })
    
  })
}
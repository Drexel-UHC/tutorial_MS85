BivariateRelationship_UI <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      div(  div(class = 'bivar-input-header',"Variable 1: Mortality"),
            InputForm_UI(ns('input1')  )),
      hr(),
      div(  div(class = 'bivar-input-header',"Variable 2: EDF/Temperature"),
            uiOutput(ns('bivar_input2')))
    ),
    mainPanel(   
      width = 9,
      tabsetPanel(
        tabPanel("Map", uiOutput(ns("sync_map")) ),
        tabPanel("Distribution of EDF/Temperature by categories of RR", UnivariateBeeswarm_UI(ns('distribution')))
      )
    )
  )
}

BivariateRelationship_Server <- function(id, data, options){
  moduleServer(id,function(input, output, session) {
    
    ### Data
    dataFiltered <- InputForm_Server('input1',data,options$bivar_metric1,options$age)
    output$bivar_input2 <- renderUI({
      ns <- session$ns
      metricTmp =  unique(dataFiltered()$metric)
      ageTmp = unique(dataFiltered()$age)
      optionsTmp = options$bivar_metric2_hot; if (str_detect(metricTmp,"cold")){   optionsTmp = options$bivar_metric2_cold}
      selectInput(ns("input2"),
                  label = "Select Metric",
                  choices = optionsTmp)
      
    })
    
    bivarData = reactive({
      req(dataFiltered())
      df_bivar = dataFiltered() %>%
        select(salid1, age, cat, metric1 = metric) %>%
        left_join(data %>% filter(metric == input$input2) %>% select(salid1,metric,age,  value,tooltip__beeswarmPlotly))
      df_bivar
    })
    
    ### Distribution
    UnivariateBeeswarm_Server('distribution',bivarData,'cat')
    
    ### Sync Map
    output$sync_map = renderUI({output$sync_map = renderUI({
      req(dataFiltered())
      req(input$input2)
      validate(need(nrow( dataFiltered() >1),"Need Data"))
      data1 = dataFiltered() 
      if (unique(data1$metric) %in% c("Relative risk at 5th percentile",'Relative risk at 95th percentile' )){
        pal <-  colorNumeric("plasma", data1$value)
        leaflet1 = leaflet(data = data1,options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addCircles(radius = 50000, weight = 1, color = "#777777",
                     fillColor = ~pal(value), fillOpacity = 0.9,
                     label = ~tooltip__map %>% map(~HTML(.x))
                     
          ) %>%
          addLegend(position = "bottomleft",
                    title = str_wrap_leaflet_legend_title(unique(data1$metric)),
                    opacity = 0.9,
                    pal = pal, values = ~value)
        
      } else {
        leaflet1 = leaflet(data = data1,options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addCircles(radius = 50000, weight = 1, color = "#777777",
                     fillColor = ~hex, fillOpacity = 0.9,
                     label = ~tooltip__map %>% map(~HTML(.x))) %>% 
          addLegend(position = "bottomleft",
                    title = str_wrap_leaflet_legend_title(unique(data1$metric)),
                    opacity = 0.9,
                    colors = options$leaflet_legend_colors, 
                    labels  =  options$leaflet_legend_labels)
      }
      
      data2 =  data %>% filter(metric == input$input2, age ==  unique(dataFiltered()$age))
      if (input$input2 == "EDF due to cold" ){
        leaflet2 = leaflet(data = data2,options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addCircles(radius = 50000, weight = 1, color = "#777777",
                     fillColor = ~hex, fillOpacity = 0.9,
                     label = ~tooltip__map %>% map(~HTML(.x))) %>% 
          addLegend(position = "bottomleft",
                    title = str_wrap_leaflet_legend_title(unique(data2$metric)),
                    opacity = 0.9,
                    colors = options$leaflet_legend_colors, 
                    labels  = data2 %>% count(cat) %>% pull(cat))
        # sort(unique(data2$cat)))
      } else {
        pal <-  colorNumeric("plasma", data2$value, reverse = grepl("EDF",input$input2))
        leaflet2 = leaflet(data = data2,options = leafletOptions(zoomControl = FALSE)) %>%
          addProviderTiles("Esri.WorldGrayCanvas") %>%
          addCircles(radius = 50000, weight = 1, color = "#777777",
                     fillColor = ~pal(value), fillOpacity = 0.9,
                     label = ~tooltip__map %>% map(~HTML(.x))
                     
          ) %>%
          addLegend(position = "bottomleft",
                    title = str_wrap_leaflet_legend_title(unique(data2$metric)),
                    opacity = 0.9,
                    pal = pal, values = ~value)
      }
      
      
      leafsync::sync(leaflet1,leaflet2, sync.cursor = F)
    })})
  })
}
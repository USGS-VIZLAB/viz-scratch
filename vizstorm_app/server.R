library(ggplot2)

shiny::shinyServer(function(input, output,session) {
  

  observe({
    if (input$close > 0) shiny::stopApp()    
  })
  
  # One way to keep track of values:
  siteDF <- reactiveValues(fileName = "Choose file",
                           stream_data = data.frame(),
                           lat_lon = data.frame())
  
  observeEvent(input$site_data,{
    path <- file.path(input$site_data$datapath)
    
    newPath <- paste0(input$site_data$datapath,"_",input$site_data$name)
    
    siteDF[["fileName"]] <- input$site_data$name
    
    newPath <- gsub(", ","_",newPath)
    
    file.rename(from = path, to = newPath)
    
    if(tools::file_ext(input$site_data$name) == "rds"){
    
      all_sites <- readRDS(newPath)

      siteDF[["stream_data"]] <- all_sites
    }
    
  })
  
  observeEvent(input$lat_lon,{
    path <- file.path(input$lat_lon$datapath)
    
    newPath <- paste0(input$lat_lon$datapath,"_",input$site_data$name)
    newPath <- gsub(", ","_",newPath)
    file.rename(from = path, to = newPath)
    
    if(tools::file_ext(input$lat_lon$name) == "rds"){
      lat_lon <- readRDS(newPath)
      siteDF[["lat_lon"]] <- lat_lon
    }
    
  })
  
  output$mymap <- leaflet::renderLeaflet({
    
    isolate({
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -83.5, lat = 44.5, zoom=6)    
    })
    
  })
  
  observe({

    sites <- unique(siteDF[["stream_data"]][["site_no"]])
    updateCheckboxGroupInput(session, "sites", 
                       choices = sites,
                       selected = sites)
  })
  
  
  output$sparks <- renderPlot({
    
    validate(
      need(nrow(siteDF[["stream_data"]]) > 0, "Please select a data set")
    )
    
    x <- siteDF[["stream_data"]]
    
    sites_to_show <- input$sites
    
    x <- filter(x, site_no %in% sites_to_show)
    
    sparklines <- ggplot(data = x) + 
      geom_line(aes(x=dateTime, y=stage_normalized),size = 1) +
      facet_grid(site_no ~ .) + 
      theme_minimal() +
      theme(axis.title =  element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            strip.text.y = element_text(angle = 0),
            panel.spacing = unit(0.0001, "lines"))
    return(sparklines)
    
  })
  
  observe({
    
    validate(
      need(nrow(siteDF[["lat_lon"]]) > 0, "Please select a data set")
    )

    mapData <- siteDF[["lat_lon"]]
    
    sites_to_show <- input$sites
    mapData <- filter(mapData, site_no %in% sites_to_show)

    map <- leaflet::leafletProxy("mymap", data=mapData) %>%
      leaflet::clearMarkers() %>%
      leaflet::addCircleMarkers(lat = ~dec_lat_va, 
                                lng = ~dec_long_va, 
                                popup = ~site_no)
  })
  
  output$downloadSites <- downloadHandler(
    
    filename = "sites.rds",

    
    content = function(file) {
      x <- siteDF[["stream_data"]]
      sites_to_show <- input$sites
      x <- filter(x, site_no %in% sites_to_show)
      saveRDS(file = file, object = x)
    }
  )
})
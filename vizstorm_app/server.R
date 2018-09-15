shiny::shinyServer(function(input, output,session) {

  observe({
    if (input$close > 0) shiny::stopApp()    
  })
  
  # One way to keep track of values:
  siteDF <- reactiveValues(fileName = "Choose file",
                           stream_data = data.frame(),
                           lat_lon = data.frame(),
                           picked_sites = NULL,
                           clicked_map_site = NULL,
                           clicked_table_site = NULL)
  
  observeEvent(input$site_data,{
    path <- file.path(input$site_data$datapath)
    
    if(all(tools::file_ext(input$site_data$name) == "rds")){
    
      x_1 <- readRDS(input$site_data$datapath[1])
      x_2 <- readRDS(input$site_data$datapath[2])

      if(ncol(x_1) > 8){
        siteDF[["stream_data"]] <- x_1
        siteDF[["lat_lon"]] <- x_2
      } else {
        siteDF[["stream_data"]] <- x_2
        siteDF[["lat_lon"]] <- x_1        
      }
    }
  })
  
  output$mymap <- leaflet::renderLeaflet({
    isolate({
      map <- leaflet::leaflet() %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::setView(lng = -82.3, lat = 34.25, zoom=6)    
    })
  })
  
  observe({
    rows_DT <- input$sitesDT_rows_selected
    sites <- siteDF[["lat_lon"]][["site_no"]]
    if(!is.null(rows_DT)){
      siteDF[["clicked_table_site"]] <-  sites[rows_DT]
    }
    siteDF[["picked_sites"]] <- unique(c(siteDF[["clicked_table_site"]],siteDF[["clicked_map_site"]]))
  })
  
  observe({
    clicked_site <- input$mymap_marker_click

    if(!is.null(clicked_site)){

      if(clicked_site$id %in% siteDF[["picked_sites"]]){
        #Right now, a single click seems to trigger this twice.
        # So, not sure how to turn off markers for now
        
        # siteDF[["picked_sites"]] <- siteDF[["picked_sites"]][!(siteDF[["picked_sites"]] %in% clicked_site$id)]
      } else {
        siteDF[["picked_sites"]] <- unique(c(siteDF[["picked_sites"]], clicked_site$id))
      }
    }
  })
  
  plot_sparks <- eventReactive(input$showSparks,{
    x <- siteDF[["stream_data"]]
    
    sites_to_show <- siteDF[["picked_sites"]]
    
    x <- filter(x, site_no %in% sites_to_show)

    sparklines <- ggplot(data = x) + 
      geom_line(aes(x=dateTime, y=X_00065_00000),size = 1) +
      facet_grid(site_no ~ ., scales = "free") + 
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
  
  output$sparks <- renderPlot({
    
    validate(
      need(nrow(siteDF[["stream_data"]]) > 0, "Please select a data set")
    )
    plot_sparks()
    
  })
  
  observe({
    
    validate(
      need(nrow(siteDF[["lat_lon"]]) > 0, "Please select a data set")
    )
    clicked_sites <- siteDF[["picked_sites"]]
    mapData <- siteDF[["lat_lon"]]
    mapData <- mapData 
    
    mapData$selected <- FALSE
    mapData$selected[mapData$site_no %in% clicked_sites] <- TRUE

    pal <- colorFactor("Blues", mapData$selected)

    map <- leaflet::leafletProxy("mymap", data=mapData) %>%
      leaflet::clearMarkers() %>%
      leaflet::addCircleMarkers(lat = ~dec_lat_va, 
                                lng = ~dec_long_va, layerId = ~site_no,
                                fillColor = ~pal(selected),
                                label = ~site_no,radius = 3,
                                fillOpacity = 0.8,opacity = 0.8,stroke=FALSE)
  })
  
  
  
  output$sitesDT <- DT::renderDataTable({
    
    validate(
      need(nrow(siteDF[["lat_lon"]]) > 0, "Please select a data set")
    )
    
    sites_dt <- siteDF[["lat_lon"]]

    sites_dt <- dplyr::select(sites_dt, site_no, station_nm, drain_area_va, flood_stage)

    DT::datatable(sites_dt,rownames = FALSE)
    
  })
  
  output$downloadSites <- downloadHandler(
    
    filename = "sites.rds",

    content = function(file) {
      x <- siteDF[["lat_lon"]]
      sites_to_show <- siteDF[["picked_sites"]]
      x <- filter(x, site_no %in% sites_to_show)
      saveRDS(file = file, object = x)
    }
  )
})
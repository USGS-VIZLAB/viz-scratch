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
                           clicked_table_site = NULL,
                           site_that_flooded = NULL)
  
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
    stream_data <- siteDF[["stream_data"]]
    site_data <- siteDF[["lat_lon"]]
    
    site_data$flood_stage <- as.numeric(site_data$flood_stage)
    stream_data <- left_join(stream_data, select(site_data, site_no, flood_stage), by="site_no")
    stream_data$flooded <- stream_data$X_00065_00000 > stream_data$flood_stage
    siteDF[["stream_data"]] <- stream_data
    siteDF[["site_that_flooded"]] <- unique(stream_data$site_no[stream_data$flooded])
    siteDF[["site_that_flooded"]] <- siteDF[["site_that_flooded"]][!is.na(siteDF[["site_that_flooded"]])]
    
    site_data$has_flooded <- site_data$site_no %in% siteDF[["site_that_flooded"]]
    siteDF[["lat_lon"]] <- site_data
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
  
  observeEvent(input$mymap_marker_click, {
    clicked_site <- input$mymap_marker_click
    
    if(is.null(clicked_site)){
      return() 
    }
    
    if(clicked_site$id %in% siteDF[["picked_sites"]]){
      siteDF[["picked_sites"]] <- siteDF[["picked_sites"]][!(siteDF[["picked_sites"]] %in% clicked_site$id)]
    } else {
      siteDF[["picked_sites"]] <- unique(c(siteDF[["picked_sites"]], clicked_site$id))
    }

    proxy %>% selectRows(which(siteDF[["lat_lon"]]$site_no %in% siteDF[["picked_sites"]]))
    
  })
  
  plot_sparks <- eventReactive(input$showSparks,{
    x <- siteDF[["stream_data"]]
    
    sites_to_show <- siteDF[["picked_sites"]]
    
    x <- filter(x, site_no %in% sites_to_show)
    
    sparklines <- ggplot(data = x) + 
      geom_line(aes(x=dateTime, y=X_00065_00000),size = 1) +
      geom_line(data = filter(x, flooded), aes(x=dateTime, y=X_00065_00000),size = 3, color = "blue") +
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

    pal <- leaflet::colorNumeric(c("red", "blue"), mapData$selected) 
 
    popup_labels <- paste0("<div style='font-size:12px;width:200px;float:left'><b>",mapData$station_nm,"</b><br/>",
                            mapData$site_no,"<br/>",
                            "<table>",
                            "<tr><td>Begin Date</td><td>",mapData$begin_date,'</td></tr>',
                            "<tr><td>End Date</td><td>",mapData$end_date,'</td></tr>',
                            "<tr><td>Drainage Area</td><td>",mapData$drain_area_va,'</td></tr>',
                            "<tr><td>Number of Samples: </td><td>",mapData$count_nu,'</td></tr>',
                            '</table></div>')
    
    mapData$labels <- sapply(popup_labels, function(x) htmltools::HTML(x))
    mapData$drain_area_va[is.na(mapData$drain_area_va)] <- min(mapData$drain_area_va, na.rm = TRUE)
    mapData$da_perc <- sapply(mapData$drain_area_va, function(x){
      ecdf(mapData$drain_area_va)(x)
    })
    mapData$count_perc <- sapply(mapData$count_nu, function(x){
      ecdf(mapData$count_nu)(x)
    })

    map <- leaflet::leafletProxy("mymap", data=mapData) %>%
      leaflet::clearMarkers() %>%
      leaflet::addCircleMarkers(lat = ~dec_lat_va, 
                                lng = ~dec_long_va, layerId = ~site_no,
                                fillColor = ~pal(selected),
                                label = ~labels,
                                # label = ~site_no,
                                radius = ~da_perc*7,
                                labelOptions = leaflet::labelOptions(textOnly = TRUE,
                                                                     style=list(
                                                                       'background'='rgba(255,255,255,0.95)',
                                                                       'border-color' = 'rgba(0,0,0,1)',
                                                                       'border-radius' = '4px',
                                                                       'border-style' = 'solid',
                                                                       'border-width' = '4px')),
                                fillOpacity = ~count_perc,
                                opacity = 0.8,
                                stroke=FALSE)
  })
  
  proxy = dataTableProxy('sitesDT')
  
  output$sitesDT <- DT::renderDataTable({
    
    validate(
      need(nrow(siteDF[["lat_lon"]]) > 0, "Please select a data set")
    )
    
    sites_dt <- siteDF[["lat_lon"]]
    sites_that_flooded <- siteDF[["site_that_flooded"]]
    
    sites_dt <- dplyr::select(sites_dt, site_no, station_nm, drain_area_va, flood_stage, has_flooded)
    
    flooded_sites <- which(sites_dt$site_no %in% sites_that_flooded)
    
    DT::datatable(sites_dt,rownames = FALSE, selection = list(selected = flooded_sites))
    
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
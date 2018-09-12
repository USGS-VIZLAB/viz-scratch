library(shiny)
library(shinydashboard)
library(dplyr)

header <- dashboardHeader(title = "Pick sites",
                          tags$li(class = "dropdown", 
                                  div(style="text-align:center;
                                      font-size: 20px;
                                      height: 50px;
                                      font-weight: 300;
                                      margin-right:25px;
                                      font-family: 'Helvetica Neue',Helvetica,Arial,sans-serif;
                                      line-height: 50px;
                                      color: #fff;")),
                          tags$li(class = "dropdown", tags$button(
                            id = 'close',
                            type = "button",
                            class = "btn action-button",
                            style='color: #000000; 
                            margin-right:13px;margin-top:7px;margin-bottom:7px',
                            onclick = "setTimeout(function(){window.close();},500);",  # close browser
                            "Stop App"
                          )))

sidebar <- dashboardSidebar(
  sidebarMenu(
    fileInput("site_data", "Load normalized_streamdata (RDS)",multiple = FALSE),
    fileInput("lat_lon", "Load all_sites (RDS)",multiple = FALSE)
  ),
  checkboxGroupInput("sites", label = NULL,
               choices = NA)
)

body <- dashboardBody(
  h3("Sites!"),

  fluidRow(
    column(6, shinycssloaders::withSpinner(leaflet::leafletOutput("mymap",height = "700px"))),
    column(6, plotOutput("sparks",height = 700,width = 600))
  ),
  downloadButton('downloadSites', 'Download RDS')

)

dashboardPage(header, sidebar, body)
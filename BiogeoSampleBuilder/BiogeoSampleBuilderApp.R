#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# library(tidyverse)
# library(sf)
# library(sp)
library(rgdal)
library(ggmap)
library(SampleBuilder)

## Input API key for Google Maps
ggmap::register_google(key = "AIzaSyBbrwI0I1mqKTZEb-OrZy2kbr3UKk2eZsI")



# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(".checkbox-inline, .radio-inline {
    text-align: center;
    margin-left: 25px;
    margin-right: 25px;
    padding: 5px;
    width: 20%;} "),
    
    title = "Biogeo Sample Builder",
    
    #### Title Section ####
    fluidRow(style='border-bottom: 1px solid black;',
             column(width=1),
             column(width=10,align="center",
                    tags$br(),
                    h2("Biogeo Sample Builder Application"),
                    "Developed by the Agroecology Laboratory - Montana State University",
                    tags$br(),
                    h6("Please contact Paul Hegedus or Bruce Maxwell for citing product. Copyright Montana State University. Updated 2021/05/27.")
             ),
             column(width=1)
    ),
    
    #### Information Section ####
    fluidRow(
        column(style='border-bottom: 1px solid black;',
               width=12,
               align="center",
               h5("This application allows users to upload shapefiles for boundaries or line features 
                  such as roads, coastlines, trails etc. From these files users can generate sample 
                  designs for their area of interest depending on their study needs. The user can 
                  select from transect or point sampling. Point sampling does not require a line 
                  feature to be uploaded by the user. The user has the option of creating a gridded
                  sampling design or a randomized sampling design. After a sampling design has 
                  been created, the user can export their sample designs for use in a GIS. The resulting
                  layers contain identifying features for each unique point/cell/transect. To add 
                  attribute features of interest to collect in the field, the user must add these 
                  in an external GIS program. Alternatively see the SampleBuilder::add_info() function 
                  in the 'SampleBuilder' R package available for download at https://github.com/paulhegedus/SampleBuilder.git")
        )
    ),
    
    #### Import Boundary Section ####
    fluidRow(
        column(style='border-bottom: 1px solid black',
               align="center",
               width=12,
               
               ## Input: Select a file
               fileInput("poly_layer", "Choose boundary shapefiles (.shp, .dbf, .sbn, .sbx, .shx, .prj, .qpj)", 
                         accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                         multiple = TRUE),
               
               ## Name area (for exporting)
               textAreaInput(inputId = "name",
                             label = "Area of Interest Name",
                             value = "", 
                             width="100%", 
                             height="28px")
        )
    ),
    
    #### Map And Tabs ####
    fluidRow(
        #### Tabs ####
        column(style='border-right: 1px solid black;',
               align="center", width=4,
               
               tabsetPanel(
                   tabPanel("Transects", 
                            h3("Transect Builder"),
                            h5("For returning a specified number of transects 
                            perpendicular  to a user supplied line feature."),
                            
                            h6("Note: transect locations are random, so every time this 
                            function is run you will receive a new set of transects. 
                            Also note, if error occurs saying 'n too small' simply rerun 
                            the function a couple times."),
                            
                            hr(),
                            
                            fileInput("line_layer", "Choose line feature shapefile for transect building (.shp, .dbf, .sbn, .sbx, .shx, .prj, .qpj)", 
                                      accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj', '.qpj'), 
                                      multiple = TRUE),
                            textInput("t_number",
                                      label = "Number of Transects",
                                      value = ""),
                            textInput("t_length",
                                      label = "Length of Transects (meters)",
                                      value = ""),
                            textInput("t_size",
                                      label = "Transect Width (meters)",
                                      value = ""),
                            selectInput("buddy_t",
                                        label = "Set to TRUE to generate a transect less than 3 
                                        transect widths away but at least 1 transect width away so 
                                        that sampling can be performed traveling away from the line 
                                        feature and back to the line feature.",
                                        choices = c(TRUE, FALSE), 
                                        selected = NULL,
                                        multiple = FALSE),
                            selectInput("direction",
                                        label = "The direction specifies which direction from the 
                                        line layer transects should be generated. Typically 
                                        'positive' indicates transects to the North and West while
                                        'negative' generates transects to the South and East. Default 
                                        is both directions from the line layer. Recommended to play 
                                        with and decide for your needs.",
                                        choices = c("Positive", "Negative"), 
                                        selected = c("Positive", "Negative"),
                                        multiple = TRUE),
                            selectInput("allow_overlaps",
                                        label = "Set to TRUE to remove areas where transects 
                                        may overlap. This restricts the segments of the line 
                                        to make transects from to areas where only a valid, 
                                        non-overlapping transect can occur. Set to FALSE to 
                                        allow transects to overlap. With low transect numbers 
                                        there is less potential for overlaps. Note that even
                                        when set to TRUE, some transects may overlap if the 
                                        number of transects is high enough.",
                                        choices = c(TRUE, FALSE), 
                                        selected = FALSE,
                                        multiple = FALSE),
                            
                            hr(),
                            
                            actionButton("buildTransects", "Build Transects"),
                            
                            hr(),
                            
                            h6("Downloading transects will export transect lines and center of transect cell centroids."),
                            actionButton("exportTransects", "Download Transects"), 
                            
                            hr()
                   ),
                   
                   tabPanel("Grid Points",
                            h3("Gridded Sample Builder"),
                            h5("Creates a grid across the area of interest and sets points 
                               at the center of each grid cell."),
                            hr(),
                            
                            textInput("cell_size",
                                      label = "Size of Grid Cells (in meters)",
                                      value = ""),
                            
                            hr(),
                            
                            actionButton("buildGridAndPnts", "Build Samples"),
                            
                            hr(),
                            
                            h6("Downloading samples will export the grid and points."),
                            actionButton("exportGridAndPnts", "Download Samples"), 
                            
                            hr()
                   ),
                   
                   tabPanel("Random Points",
                            h3("Random Point Builder"),
                            h5("Randomly applies samples across the specified area."),
                            hr(),
                            
                            textInput("n",
                                      label = "Number of samples.",
                                      value = ""),
                            
                            hr(),
                            
                            actionButton("buildRandPnts", "Build Samples"),
                            
                            hr(),
                            
                            actionButton("exportRandPnts", "Download Samples"), 
                            
                            hr()
                   )
               )
        ),
        
        #### Map ####
        column(style = 'border-left: 1px solid black;',
               align = "center", 
               width = 8,
               tabPanel("Map", plotOutput("map", width = "100%",height=650))
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    dat <- reactiveValues(poly_layer = NULL,
                           name = NULL, 
                           line_layer = NULL)  
    plotCode <- reactiveValues(ord = 0)
    pType <- reactiveValues(type = 0)
    
    
    ## Get shapefile input for poly layer
    observeEvent(input$poly_layer, {
        browser()
        
        plotCode$ord <- 1
        
        poly_layer <- input$poly_layer
        if(is.null(poly_layer)){
            return()
        }
        previouswd <- getwd()
        uploaddirectory <- dirname(poly_layer$datapath[1])
        setwd(uploaddirectory)
        for(i in 1:nrow(poly_layer)){
            file.rename(poly_layer$datapath[i], poly_layer$name[i])
        }
        setwd(previouswd)
        
        map <- rgdal::readOGR(paste(uploaddirectory, 
                                    poly_layer$name[grep(pattern="*.shp$", poly_layer$name)], 
                                    sep="/")) %>% #,  delete_null_obj=TRUE) 
            sp::spTransform(CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) %>% 
            sf::st_as_sf()
        utm_epsg <- calcUTMzone(map)
        map <- sf::st_transform(map, crs = sf::st_crs(4326))
       
        dat$poly_layer <- map
    })
    
    polyDatasetInput <- reactive({
        browser()
        
        req(input$poly_layer)
        react_dat <- list()
        react_dat$spdat <- as(dat$poly_layer, "Spatial")
        react_dat$longlat <- coordinates(react_dat$spdat)
        
        return(react_dat)
    })
    
    ## Get name of boundary area
    observeEvent(input$name, {
        dat$name <- input$name
    })
    
    
    ## Show map
    output$map <- renderPlot({
        if(plotCode$ord != 0){
            if (plotCode$ord == 1) {
                plotPolyLayer()
            }
        }else{
            map <- get_map(location = c(lon = -103.461760283, lat = 44.58207622), 
                           zoom = 3,
                           maptype = "hybrid", source = "google")
            baseMap <- suppressMessages(ggmap(map,extent = "panel") +
                                            geom_polygon(data=map_data("usa"), 
                                                         aes(x=long, y=lat, group=group), col="yellow",fill=NA)) +
                labs(x="", y="") +
                theme(axis.text.x=element_blank(),
                      axis.text.y=element_blank()) 
            suppressWarnings(print(baseMap)) 
        }
    })
    
    ## Mapping Function
    
    plotPolyLayer <- function() {
        browser()
        
        ## if only poly layer
        map <- get_map(location = c(lon = mean(coordinates(polyDatasetInput()$longlat)[,1]), 
                                    lat = mean(coordinates(polyDatasetInput()$longlat)[,2])), 
                       zoom = 14,
                       maptype = "satellite", source = "google")
        baseMap <- suppressMessages(ggmap(map,extent = "panel") +
                                        geom_polygon(data=map_data("usa"), 
                                                     aes(x=long, y=lat, group=group), col="yellow",fill=NA)) +
            labs(x="", y="") +
            theme(axis.text.x=element_blank(),
                  axis.text.y=element_blank()) 
        suppressWarnings(print(baseMap)) 
        ## if build transects
        
        
        ## if build grid
        
        
        ## if build rand pts
        
    }
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

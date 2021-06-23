
library(RColorBrewer)
library(shiny) # Main library
library(shinydashboard) # Pretty dashboard layouts
library(ggplot2) # Plots
library(dplyr) # Data manipulate
library(babynames) # Data set
library(gapminder)
library(plotly)
library(DT)
library(plotly)
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
#library(spDataLarge)   # load larger geographic data
library(maps)
library(mapproj)
library(purrr)
library(dplyr)
library(leaflet)
library(leaflet.extras)


# Load data ####
species <- read.csv(file = 'data/species.csv')
parks <- read.csv(file = 'data/parks.csv')
# Prepare data ####
status_to_keep <- c('Extinct', 'Endangered', 'Threatened', 'In Recovery', 
                    'Proposed Endangered', 'Proposed Threatened', 'Species of Concern')
species <- filter(species, Conservation.Status %in% status_to_keep)
features_to_keep <- c('Park.Name', 'Category', 'Order', 'Family', 'Common.Names',
                      'Abundance', 'Conservation.Status')
species <- species[features_to_keep]
status_list <- unique(species$Conservation.Status)
category_list <- unique(species$Category)

# Create dashboard UI #### 
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Biodiversity in USA National Parks", titleWidth = 350),
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Species status", tabName = "status", icon = icon("dashboard")),
            menuItem("Species abundance", tabName = "abundance", icon = icon("dashboard")),
            menuItem("Maps", tabName = "map", icon = icon("map")),
            menuItem("Source data tables", tabName = "tables", icon = icon("table")),
            menuItem("Info", tabName = "info", icon = icon("info-circle"))
        )
    ),
    dashboardBody(
        tabItems(
            # Home tab content
            tabItem(tabName = "home",
                    fillPage(
                        #includeCSS("www/nav_padding.css"),
                        fluidRow(
                            img(src="biodiversity-banner.jpg", width="100%")
                        ),
                        fluidRow(
                            style = "margin-left: 50px; margin-right: 50px; margin-top: 20px; margib-bottom: 40px;",
                            box(
                                status = "success",
                                solidHeader = FALSE,
                                width = "100%",
                                title = "About",
                                "This dashboard visualizes the data from the datasate about biodiversity in USA National Parks, created by National Park Service. 
                                Data contains informations about all plant and animal species found in the American national park system. 
                                This dashbord focuses only on endangered or extinct species."
                            ),
                            align="center"
                        ),
                        fluidRow(
                            column(12,
                                   fluidRow(
                                      column(4, offset=1, align="center",
                                             box(
                                                 width = "100%",
                                                 status = "warning",
                                                 solidHeader = TRUE,
                                                 title = "Data about species",
                                                 htmlOutput("home_species_data_text")
                                             )
                                             ),
                                      column(4, offset=2, align="center",
                                             box(
                                                 width = "100%",
                                                 status = "info",
                                                 solidHeader = TRUE,
                                                 title = "Data about parks",
                                                 htmlOutput("home_parks_data_text")  
                                             )
                                             )
                                   )
                            )
                        )
                    )
            ),
            
            # Status tab content
            tabItem(tabName = "status",
                    fluidRow(
                        column(width = 6,
                               box(
                                   status = "primary", width = NULL,
                                   selectInput('status_status',
                                               'Select status',
                                               choices = status_list
                                   )
                               )
                        ),
                        column(width = 6,
                               box(
                                   status = "info", width = NULL,
                                   selectInput('status_category',
                                               'Select category',
                                               choices = category_list
                                   )
                               )
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               box(
                                   title = "Status and families",
                                   status = "success", solidHeader = TRUE, width = NULL,
                                   plotOutput('status_status_category_plot')
                               )
                        )
                    ),
                    fluidRow(
                        infoBoxOutput("status_totalspecies"),
                        infoBoxOutput("status_smallest_family"),
                        infoBoxOutput("status_biggest_family")
                    ),
                    fluidRow(
                        column(width = 12,
                            box(
                                title = "Proportions of conservation statuses",
                                status = "primary", solidHeader = TRUE, width = NULL,
                                plotOutput('status_status_category_plot_stacked')
                            )
                        )
                    )
            ),
            # Abundance tab content
            tabItem(tabName = "abundance",
                    fluidRow(
                        column(width = 12,
                            box(
                                status = "info", width = NULL,
                                selectInput('abundance_category',
                                            'Select category',
                                            choices = category_list
                                )
                            )
                        )
                    ),
                    fluidRow(
                        column(width = 12,
                               box(
                                   title = "Abundance of families",
                                   status = "primary", solidHeader = TRUE, width = NULL,
                                   plotOutput('abundance_bar_abundance')
                               )
                        )
                    ),
                    fluidRow(
                        column(width = 6,
                               box(
                                   title = "Abundance of all categories",
                                   status = "success", solidHeader = TRUE, width = NULL,
                                   plotOutput('abundance_circle_abundance_all')
                               )
                        ),
                        column(width = 6,
                               box(
                                   title = "Abundance of choosen categories",
                                   status = "warning", solidHeader = TRUE, width = NULL,
                                   plotOutput('abundance_circle_abundance_single')
                               )
                        )
                    )
                ),
            
            # Map tab content
            
            tabItem(tabName = "map",
              
                  fluidRow(
                  column(width = 4,
                         height = 5,
                         box(
                           solidHeader=TRUE,
                           title = "Parks Area",
                           status = "danger", width = NULL,
                           plotOutput('map1')
                         )
                  ),
                  
                  column(width = 4,
                         height = 5,
                         box(
                           solidHeader=TRUE,
                           title = "Parks Names",
                           status = "success", width = NULL,
                           plotOutput('map2')
                         )
                  ),
                  column(width = 4,
                         height = 5,
                         box(
                           solidHeader=TRUE,
                           title = "Parks Location",
                           status = "info", width = NULL,
                           plotOutput('map3')
                         )
                    )
                  ),
                  fluidRow(
                    infoBoxOutput("lowest_area"),
                    infoBoxOutput("max_area"),
                    infoBoxOutput("most_parks_in_state")
                  ),
                  fluidRow(
                    column(width = 4,
                           box(
                             solidHeader=TRUE,
                             title = "Species in parks - controls",
                             status = "danger", width = NULL,
                             sliderInput("map_species_range", "Species", min=21, max=220,
                                         value = c(21,220), step = 1
                             )
                           )
                    ),
                    column(
                      width = 4,
                      box(
                        solidHeader=TRUE,
                        title = "Species in parks",
                        status = "success", width = NULL,
                        leafletOutput('map_species')
                      )
                    ),
                    column(
                      width = 4,
                      box(
                        solidHeader=TRUE,
                        title = "Parks",
                        status = "info", width = NULL,
                        DT::dataTableOutput('parks_table')
                      )
                    )
                  )

            ),
            # Tables tab content
            tabItem(tabName = "tables",
                      tabsetPanel(tabPanel('Species',
                                           DT::dataTableOutput('tables_speciesTable')),
                                  tabPanel('Parks',
                                           DT::dataTableOutput('tables_parksTable')
                                           )
                      )
                    ),
            # Info tab content
            tabItem(tabName = "info",
                    fluidRow(
                        infoBoxOutput("info_authors"),
                        infoBoxOutput("info_datasource"),
                        infoBoxOutput("info_version")
                    )
                )
            
        )
    )
)

# Create dashboard logic ####
server <- function(input, output) {
    # HOME PAGE
    output$home_species_data_text <- renderUI({
        HTML(paste("Name of the park in which species lives.", "Species category.", 
              "Species order.", "Species family.", "Species common names.", 
              "Species abudance.", "Species status.", sep="<br/>"))
    })
    output$home_parks_data_text <- renderUI({
        HTML(paste("Park code.", "Park name.", "State in which park is located", 
                   "Park area in acres.", "Park geographical coordinates", sep="<br/>"))
    })
    # STATUS
    output$status_status_category_plot <- renderPlot({
        species %>%
            filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
            group_by(Family, Order) %>%
            summarise(Count = n()) %>%
            # plot_ly(fill = ~Family, x = ~Order, y = ~Count, 
            #         name = paste(input$status_status, " - " , input$status_category)) %>%
            # layout(barmode = 'group')
            ggplot(aes(fill = Family, x = Order, y = Count)) +
            geom_bar(position=position_dodge2(), stat = "identity") +
            ggtitle(paste(input$status_status, " - " , input$status_category))
    })
    output$status_totalspecies <- renderInfoBox({
        species_status <- species %>%
            filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
            group_by(Family, Order) %>%
            summarise(Count = n())
        species_status_a <- species %>%
            summarise(Count = n())
        infoBox(
            "Total species", 
            HTML(paste(sum(species_status$Count), "/", species_status_a$Count, br(), round((sum(species_status$Count) / species_status_a$Count) * 100, 1), "%")),
            icon = icon("list"),
            color = "green"
        )
    })
    output$status_smallest_family <- renderInfoBox({
        species_status <- species %>%
            filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
            group_by(Family, Order) %>%
            summarise(Count = n())
        infoBox(
            "Smallest family", 
            HTML(paste(min(species_status$Count), br(), species_status$Family[which.min(species_status$Count)])), 
            icon = icon("arrow-circle-down"),
            color = "yellow"
        )
    })
    output$status_biggest_family <- renderInfoBox({
        species_status <- species %>%
            filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
            group_by(Family, Order) %>%
            summarise(Count = n())
        infoBox(
            "Biggest family", 
            HTML(paste(max(species_status$Count), br(), species_status$Family[which.max(species_status$Count)])), 
            icon = icon("arrow-alt-circle-up"),
            color = "fuchsia"
        )
    })
    output$status_status_category_plot_stacked <- renderPlot({
        species %>%
            filter(Conservation.Status != "Species of Concern") %>%
            group_by(Category, Conservation.Status) %>%
            summarise(Count = n()) %>%
            ggplot(aes(fill = Conservation.Status, x = Category, y = Count)) +
                geom_bar(position="stack", stat = "identity") +
                coord_flip() +
                scale_fill_manual(values = c("#FFDB6D", "#C4961A", "#D16103", "#C3D7A4", "#4E84C4", "#52854C"))
    })
    
    # ABUNDANCE
    output$abundance_bar_abundance <- renderPlot({
        abundance_to_keep <- c('Abundant', 'Common', 'Uncommon', 'Occasional', 'Rare')
        species %>%
            filter(Abundance %in% abundance_to_keep, Category == input$abundance_category) %>%
            group_by(Order, Abundance) %>%
            summarise(Count = n()) %>%
            ggplot(aes(fill = Abundance, x = Order, y = Count)) +
            geom_bar(position="stack", stat = "identity")
    })
    output$abundance_circle_abundance_all <- renderPlot(res = 200, {
        greens_palette <- brewer.pal(name="Greens",n=9)[3:7]
        abundance_to_keep <- c('Abundant', 'Common', 'Uncommon', 'Occasional', 'Rare')
        species_temp <- species %>%
            filter(Abundance %in% abundance_to_keep) %>%
            group_by(Abundance) %>%
            summarise(Count = n())
        # Compute percentages
        species_temp$fraction = species_temp$Count / sum(species_temp$Count) 
        # Compute the cumulative percentages (top of each rectangle)
        species_temp$ymax = cumsum(species_temp$fraction)
        # Compute the bottom of each rectangle
        species_temp$ymin = c(0, head(species_temp$ymax, n=-1)) 
        # Compute label position
        species_temp$labelPosition <- (species_temp$ymax + species_temp$ymin) / 2
        # Compute a good label
        species_temp$label <- paste0(species_temp$Abundance, "\n", species_temp$Count)
        species_temp %>%
            ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Abundance)) +
                geom_rect() +
                # x here controls label position (inner / outer)
                geom_text( x=1.5, aes(y=labelPosition, label=label, color=Abundance), size=2) + 
                scale_fill_manual(values=greens_palette) +
                scale_color_manual(values=greens_palette) +
                coord_polar(theta="y") +
                xlim(c(-1, 4)) +
                theme_void() +
                theme(legend.position = "none")
    })
    output$abundance_circle_abundance_single <- renderPlot(res = 200, {
        oranges_palette <- brewer.pal(name="Oranges",n=9)[3:7]
        #my_palette <- c(brewer.pal(name="Greens",n=9)[2], brewer.pal(name="Greens",n=9)[3], brewer.pal(name="Greens",n=9)[5], brewer.pal(name="Greens",n=9)[7], brewer.pal(name="Greens",n=9)[9])
        abundance_to_keep <- c('Abundant', 'Common', 'Uncommon', 'Occasional', 'Rare')
        species_temp <- species %>%
            filter(Abundance %in% abundance_to_keep, Category == input$abundance_category) %>%
            group_by(Abundance) %>%
            summarise(Count = n())
        # Compute percentages
        species_temp$fraction = species_temp$Count / sum(species_temp$Count) 
        # Compute the cumulative percentages (top of each rectangle)
        species_temp$ymax = cumsum(species_temp$fraction)
        # Compute the bottom of each rectangle
        species_temp$ymin = c(0, head(species_temp$ymax, n=-1)) 
        # Compute label position
        species_temp$labelPosition <- (species_temp$ymax + species_temp$ymin) / 2
        # Compute a good label
        species_temp$label <- paste0(species_temp$Abundance, "\n", species_temp$Count)
        species_temp %>%
            ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Abundance)) +
            geom_rect() +
            # x here controls label position (inner / outer)
            geom_text( x=1.5, aes(y=labelPosition, label=label, color=Abundance), size=2) + 
            scale_fill_manual(values=oranges_palette) +
            scale_color_manual(values=oranges_palette) +
            coord_polar(theta="y") +
            xlim(c(-1, 4)) +
            theme_void() +
            theme(legend.position = "none")
    })
    
    # MAPS
    
    output$map2 <- renderPlot({
      ggplot(parks, aes(x=Longitude, y=Latitude)) +   
      borders("world", colour=NA, fill="olivedrab3")  +
      scale_x_continuous(name="Longitude", limits=c(-180, -20)) +
      scale_y_continuous(name="Latitude", limits=c(0, 80)) +
      theme(panel.background = element_rect(fill = "lightblue", colour = "azure1")) +
      geom_text(aes(x=Longitude, y= Latitude, label=Park.Code),
                color = "gray20", check_overlap = T, size = 3)
      
    })
    
    output$map1 <- renderPlot({
      ggplot(parks, aes(x=Longitude, y= Latitude)) +   
        borders("world", colour=NA, fill="wheat1")  +
        geom_point(color="blue", alpha = .2, size = parks$Acres/500000) +
        scale_x_continuous(name="Longitude", limits=c(-170, -30)) +
        scale_y_continuous(name="Latitude", limits=c(0, 80)) +
        theme(panel.background = element_rect(fill = "azure1", colour = "azure1")) +
        geom_text(aes(x=Longitude, y= Latitude, label=""),
                  color = "gray20", check_overlap = T, size = 3)
    })
    
    output$map3 <- renderPlot({
      ggplot(parks, aes(x=Longitude, y= Latitude)) +   
        borders("world", colour=NA, fill="wheat1") +
        borders("state", colour="white", fill=NA) +
        geom_point(color="blue", alpha = .2, size = 3) +
        scale_x_continuous(name="Longitude", limits=c(-170, -30)) +
        scale_y_continuous(name="Latitude", limits=c(0, 80)) +
        theme(panel.background = element_rect(fill = "azure1", colour = "azure1")) +
        geom_text(aes(x=Longitude, y= Latitude, label=""),
                  color = "gray20", check_overlap = T, size = 3)
    })
    
    output$parks_table <- DT::renderDataTable({
      parks_small <- parks[c("Park.Code", "Park.Name", "Acres")]
      parks_small %>%
        DT::datatable(filter = "top")
    })
    
    
    output$max_area <- renderInfoBox({
      infoBox(
        "Largest Park", HTML(paste("Wrangell - St Elias National Park and Preserve",br(), "8323148 Acres")), icon = icon("arrow-alt-circle-up"),
        color = "olive"
      )
    })
    
    output$lowest_area <- renderInfoBox({
      infoBox(
        "Smallest Park", HTML(paste("Hot Springs National Park",br(), "5550 Acres")), icon = icon("arrow-circle-down"),
        color = "maroon"
      )
    })
    output$most_parks_in_state <- renderInfoBox({
      infoBox(
        "State with most parks", HTML(paste("Alaska",br(), "8")), icon = icon("calculator"),
        color = "light-blue"
      )
    })
    
    species_parks_temp <- species %>%
      group_by(Park.Name) %>%
      summarise(Species.Count = n())
    
    species_parks <- merge(x = species_parks_temp, y = parks)
    
    filteredData <- reactive({
      species_parks[species_parks$Species.Count >= input$map_species_range[1] & species_parks$Species.Count <= input$map_species_range[2],]
    })
    
    
    output$map_species <- renderLeaflet({
      map_pal <- colorFactor(
        palette = 'Spectral',
        domain = species_parks$Species.Count
      )
      leaflet(species_parks) %>% addTiles() %>%
        fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude)) %>%
        addCircles(data = species_parks, lat = ~ Latitude, lng = ~ Longitude,
                                  weight = 1, radius = ~Species.Count*1000, popup = ~as.character(Species.Count),
                                  label = ~as.character(paste0(Park.Name, ": ", Species.Count)),
                                  color = ~map_pal(Species.Count), fillOpacity = 0.7)
    })
    
    observe({
      map_pal <- colorFactor(
        palette = 'Spectral',
        domain = species_parks$Species.Count
      )
      
      leafletProxy("map_species", data = filteredData()) %>%
        clearShapes() %>%
        fitBounds(~min(Longitude)-10, ~min(Latitude)-10, ~max(Longitude)+10, ~max(Latitude)+10) %>%
        addCircles(lat = ~ Latitude, lng = ~ Longitude,
                                  weight = 1, radius = ~Species.Count*1000,
                                  label = ~as.character(paste0(Park.Name, ": ", Species.Count)),
                                  color = ~map_pal(Species.Count), fillOpacity = 0.7)
    })
  
    # TABLES
    output$tables_speciesTable <- DT::renderDataTable({
        species %>%
            DT::datatable(filter = "top")
    })
    output$tables_parksTable <- DT::renderDataTable({
        parks %>%
            DT::datatable(filter = "top")
    })
    # INFO
    output$info_authors <- renderInfoBox({
        infoBox(
            "Authors", HTML(paste("Piotr Kopycki",br(), "Szymon Szczot")), icon = icon("user-friends"),
            color = "navy"
        )
    })
    output$info_datasource <- renderInfoBox({
        infoBox(
            "Data source", paste0("https://www.kaggle.com/nationalparkservice/park-biodiversity"), icon = icon("database"),
            color = "orange"
        )
    })
    output$info_version <- renderInfoBox({
        infoBox(
            "Version", paste0("v1.0/2021"), icon = icon("code"),
            color = "maroon"
        )
    })
}

# Run dashboard ####
shinyApp(ui = ui, server = server)

library(shiny) # Main library
library(shinydashboard) # Pretty dashboard layouts
library(ggplot2) # Plots
library(dplyr) # Data manipulate
library(babynames) # Data set
library(gapminder)
install.packages("plotly")
library(plotly)
library(DT)
library(plotly)
library(sf)          # classes and functions for vector data
library(raster)      # classes and functions for raster data
library(spData)        # load geographic data
library(spDataLarge)   # load larger geographic data
library(maps)
library(mapproj)
library(purrr)
library(dplyr)
install.packages(c("maps", "mapproj"))


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
                         title = "Status and families in categories",
                         status = "success", width = NULL,
                         plotOutput('status_status_category_plot')
                       )
                )
              ),
              fluidRow(
                infoBoxOutput("status_totalspecies"),
                infoBoxOutput("status_percentofall"),
                infoBoxOutput("status_biggest_family")
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "Proportions of endangered status in categories without species of concern",
                         status = "warning", width = NULL,
                         plotOutput('status_status_category_plot_stacked')
                       )
                )
              )
      ),
      # Abundance tab content
      tabItem(tabName = "abundance",
              fluidRow(
                box(
                  status = "info", width = NULL,
                  selectInput('abundance_category',
                              'Select category',
                              choices = category_list
                  )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         title = "Abundance of families",
                         status = "primary", width = NULL,
                         plotOutput('abundance_bar_abundance')
                       )
                )
              ),
              fluidRow(
                column(width = 6,
                       box(
                         title = "Abundance of all categories",
                         status = "success", width = NULL,
                         plotOutput('abundance_circle_abundance_all')
                       )
                ),
                column(width = 6,
                       box(
                         title = "Abundance of choosen categories",
                         status = "warning", width = NULL,
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
                       status = "primary", width = NULL,
                       plotOutput('map1')
                     )
              ),
              
              column(width = 4,
                     height = 5,
                     box(
                       solidHeader=TRUE,
                       title = "Parks Names",
                       status = "primary", width = NULL,
                       plotOutput('map2')
                     )
              ),
              column(width = 4,
                     height = 5,
                     box(
                       solidHeader=TRUE,
                       title = "Parks Location",
                       status = "warning", width = NULL,
                       plotOutput('map3')
                     )
                )
              ),
              fluidRow(
                infoBoxOutput("max_area"),
                infoBoxOutput("lowest_area"),
                infoBoxOutput("most_parks_in_state")
              ),
              fluidRow(
                width = 8,
                tabsetPanel(tabPanel('Parks',
                                     DT::dataTableOutput('parks_table'))
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
    infoBox(
      "Total species", paste0(sum(species_status$Count)), icon = icon("list"),
      color = "purple"
    )
  })
  output$status_percentofall <- renderInfoBox({
    species_status <- species %>%
      filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
      group_by(Family, Order) %>%
      summarise(Count = n())
    species_status_a <- species %>%
      summarise(Count = n())
    perc <- sum(species_status$Count)/species_status$Count
    infoBox(
      "Percentage of all species", paste0(perc, "%"), icon = icon("list"),
      color = "purple"
    )
  })
  output$status_biggest_family <- renderInfoBox({
    species_status <- species %>%
      filter(Conservation.Status == input$status_status, Category == input$status_category) %>%
      group_by(Family, Order) %>%
      summarise(Count = n())
    infoBox(
      "Biggest family", paste0(max(species_status$Count)), icon = icon("list"),
      color = "teal"
    )
  })
  output$status_status_category_plot_stacked <- renderPlot({
    species %>%
      filter(Conservation.Status != "Species of Concern") %>%
      group_by(Category, Conservation.Status) %>%
      summarise(Count = n()) %>%
      ggplot(aes(fill = Conservation.Status, x = Category, y = Count)) +
      geom_bar(position="stack", stat = "identity") +
      coord_flip()
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
  output$abundance_circle_abundance_all <- renderPlot({
    abundance_to_keep <- c('Abundant', 'Common', 'Uncommon', 'Occasional', 'Rare')
    species_temp <- species %>%
      filter(Abundance %in% abundance_to_keep) %>%
      group_by(Order, Abundance) %>%
      summarise(Count = n())
    species_temp$fraction = species_temp$Count / sum(species_temp$Count) 
    species_temp$ymax = cumsum(species_temp$fraction)
    species_temp$ymin = c(0, head(species_temp$ymax, n=-1)) 
    species_temp %>%
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Abundance)) +
      geom_rect() +
      coord_polar(theta="y")
    # summarise(Ymax = cumsum(Fraction)) %>%
    # summarise(Ymin = c(0, head(Ymax, n=-1))) %>%
    # summarise(LabelPosition = (Ymax + Ymin) / 2) %>%
    # summarise(Label = paste0(Abundance, "\n value: ", Count)) %>%
    # ggplot(aes(ymax=Ymax, ymin=Ymin, xmax=4, xmin=3, fill=Abundance)) +
    #     geom_rect() +
    #     geom_text( x=2, aes(y=LabelPosition, label=Label, color=Abundance), size=6) + # x here controls label position (inner / outer)
    #     scale_fill_brewer(palette=3) +
    #     scale_color_brewer(palette=3) +
    #     coord_polar(theta="y") +
    #     xlim(c(-1, 4)) +
    #     theme_void() +
    #     theme(legend.position = "none")
    # ggplot(aes(fill = Abundance, x = Order, y = Count)) +
    #     geom_bar(position="stack", stat = "identity")
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
    parks %>%
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
      color = "orange"
    )
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
      "Version", paste0("v1.0_2021"), icon = icon("code"),
      color = "maroon"
    )
  })
}

# Run dashboard ####
shinyApp(ui = ui, server = server)

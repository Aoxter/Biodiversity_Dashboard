library(shiny) # Main library
library(shinydashboard) # Pretty dashboard layouts
library(ggplot2) # Plots
library(dplyr) # Data manipulate
library(babynames) # Data set
library(gapminder)
library(DT)

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

# Create dashboard UI #### 
ui <- dashboardPage(
    skin = "green",
    dashboardHeader(title = "Biodiversity in USA National Parks", titleWidth = 350),
    dashboardSidebar(
        width = 350,
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Page 1", tabName = "page1", icon = icon("dashboard")),
            menuItem("Page 2", tabName = "page2", icon = icon("dashboard")),
            menuItem("Page 3", tabName = "page3", icon = icon("dashboard")),
            menuItem("Page 4", tabName = "page4", icon = icon("dashboard")),
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
            
            # Second tab content
            tabItem(tabName = "page1")
        )
    )
)
# ui <- fluidPage(
#   titlePanel("Life Expactation and GDP per Capita"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput('continent',
#                   'Select continent',
#                   choices = continents),
#       sliderInput('year',
#                   'Select year',
#                   min = 1952,
#                   max = 2007,
#                   value = 1972)
#     ),
#     mainPanel(
#       tabsetPanel(tabPanel('Plot',
#                            plotOutput('PlotTopNames')),
#                   tabPanel('Table',
#                            DT::dataTableOutput('tableNames'))
#       )
#     )
#   )
# )
# Create dashboard logic ####
server <- function(input, output) {
    output$home_species_data_text <- renderUI({
        HTML(paste("Name of the park in which species lives.", "Species category.", 
              "Species order.", "Species family.", "Species common names.", 
              "Species abudance.", "Species status.", sep="<br/>"))
    })
    output$home_parks_data_text <- renderUI({
        HTML(paste("Park code.", "Park name.", "State in which park is located", 
                   "Park area in acres.", "Park geographical coordinates", sep="<br/>"))
    })
}

# server <- function(input, output, session){
#   output$tableNames <- DT::renderDataTable({
#     gapminder %>%
#       filter(continent == input$continent) %>%
#       filter(year == input$year) %>%
#       DT::datatable()
#   })
#   
#   output$PlotTopNames <- renderPlot({
#     gapminder %>%
#       filter(continent == input$continent, year == input$year) %>%
#       ggplot(aes(x = gdpPercap, y = lifeExp)) +
#       geom_point(size=2, shape=16)
#   })
# }
# Run dashboard ####
shinyApp(ui = ui, server = server)

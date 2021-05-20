# Wine Browser application

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(data.table)
library(plotly)


ui <- dashboardPage(skin = "red",
                    dashboardHeader(
                        title = 'Wine Browser'
                    ),
                    
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem('Filters', tabName = 'filters', icon = icon("filter")),
                            menuItem('Visualization', tabName = 'visual', icon = icon("wine-glass-alt")),
                            menuItem('Data', tabName = 'data', icon = icon("table"))
                        )
                    ), # end sidebar
                    
                    dashboardBody(
                        tabItems(
                            tabItem(tabName = 'filters',
                                    fluidRow(
                                        column(4,
                                               checkboxGroupInput('wine_type', label = h3('Wine Type'), choices = wine_types, selected = wine_types),
                                               br(),
                                               pickerInput('country', label = h3('Country of Origin'), choices = countries, selected = countries, options = list(`actions-box` = TRUE), multiple = TRUE),
                                               br(),
                                               tags$h3('Click here to reset filters'),
                                               actionButton('button_reset', label = 'Reset Filters')
                                        ), 
                                        column(6,
                                               sliderInput(inputId = "price_range", label = h3("Price of Bottle in EUR"), min = 0, max = ceiling(max(wine_data$Price)),
                                                           step = 10, round = TRUE, value = c(0, ceiling(max(wine_data$Price)))),
                                               sliderInput(inputId = "rating_range", label = h3("Rating of Wine"), min = 1, 
                                                           max = 5, value = c(1, 5), step = 0.1),
                                               sliderInput(inputId = "year_range", label = h3("Year of Wine"), min = 1960, 
                                                           max = year(Sys.Date()), value = c(1960, 2021))
                                        )
                                    )
                            ), # End filters tab
                            
                            tabItem(tabName = 'visual',
                                    fluidRow(infoBoxOutput('nr_infobox'),
                                             infoBoxOutput('price_infobox'),
                                             infoBoxOutput('nr_rating_infobox')
                                    ),
                                    fluidRow(column(6,plotlyOutput('type_rating_plot')),
                                             column(6,plotlyOutput('rating_plot'))
                                    ),
                                    br(),
                                    ### Did not include it as it significantly slows down the application
                                    # fluidRow(plotlyOutput('price_rating_plot')
                                    # ),
                                    # br(),
                                    fluidRow(plotlyOutput('year_price_plot')
                                    ),
                                    br(),
                                    fluidRow(plotlyOutput('country_rating_plot')
                                    ),
                            ), # End visual tab
                            
                            tabItem(tabName = 'data',
                                    DT::dataTableOutput('my_data')
                            ) # End data tab
                        )# end tab items
                    ) # end body
)


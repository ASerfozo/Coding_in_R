# Wine Browser application

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(data.table)
library(DT)
library(ggplot2)
library(plotly)


# Import data -------------------------------------------------------------

wine_data <- read.csv("https://raw.githubusercontent.com/ASerfozo/Data_Analysis_in_R/main/Task_8_wine_browser_shiny/wine_data.csv", stringsAsFactors = FALSE)
wine_data <- wine_data %>% mutate(lnPrice = round(log(Price),3))
wine_types <- unique(wine_data$Type)
countries <- unique(wine_data$Country)

# Function for boxplots ---------------------------------------------------

get_boxplot <- function(df,x,y) {
  ggplot(df, aes(factor(x),y)) +
    geom_boxplot(color = "black", fill = "red3", alpha = 0.5) +
    theme_bw()
}

# Function for filtering data -------------------------------------------------

get_data_by_year <- function(type, countryof, start_price, end_price,
                             start_rating, end_rating, start_year, end_year) {
  my_data <- filter(wine_data, 
                    Type %in% type,
                    Country %in% countryof,
                    Price > start_price, Price <= end_price,
                    Rating >= start_rating, Rating <= end_rating,
                    Year >= start_year, Year <= end_year
  )
  return(my_data)
}
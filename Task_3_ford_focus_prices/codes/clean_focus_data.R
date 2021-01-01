##########################################
##             Data Analysis 2          ##
##              Term Project            ##
##             Attila Serfozo           ##
##               Clean Data             ##
##########################################

rm(list=ls())
library(tidyverse)

# Import raw data
data_in <- "https://raw.githubusercontent.com/ASerfozo/Coding_in_R/main/Task_3_ford_focus_prices/data/Raw/ford_focus_scraping.csv"
focusdb <- read_csv(data_in)

# Remove unnecessary variables left from scraping
focusdb <- select ( focusdb, -c(Pagination , `web-scraper-start-url`, `Pagination-href`, Open_element) )

# Create an ID from web-scraper-order variable
focusdb <- separate ( focusdb , `web-scraper-order`, "-" ,
                      into = c("garbage" ,"ID"))
focusdb <- select ( focusdb , -garbage)
# Convert ID to numeric values
focusdb$ID <- as.numeric( focusdb$ID)
typeof(focusdb$ID)

# Order table by ID number
focusdb <- arrange(focusdb , ID)

# Separate the Performance variables kW and horsepower
focusdb <- separate(focusdb, Performance, "," ,
                    into = c("Performance_kW", "Performance_HP"))

# Mutate all measures
focusdb <- mutate(focusdb, 
                 Price = as.numeric(gsub("[^0-9]","",Price ) ),
                 Kilometers = as.numeric(gsub("[^0-9]","",Kilometers ) ),
                 Cylinder_capacity = as.numeric(gsub("[^0-9]","",Cylinder_capacity ) ),
                 Performance_kW = as.numeric(gsub("[^0-9]","",Performance_kW ) ),
                 Performance_HP = as.numeric(gsub("[^0-9]","",Performance_HP ) ))

# Rename variables
focusdb <- rename(focusdb, 
                  Price_HUF = Price,
                  Cylinder_capacity_cm3 = Cylinder_capacity,
                  Link = `Open_element-href`)

# Filter out observations with missing prices
focusdb <- filter(focusdb, 
                  Price_HUF != "NA",
                  Fuel_type != "Elektromos",
                  Fuel_type !="Benzin/Gáz")

# Converting Transmission observations to Manual/Automatic
focusdb <- mutate(focusdb, Transmission = ifelse(focusdb$Transmission == "Manuális (6 fokozatú)", "Manuális", 
                                  ifelse(focusdb$Transmission == "Manuális (5 fokozatú)", "Manuális", "Automata") ) )

## Converting Registration date to Registration year and creating Age variable
# Separating year and month
focusdb <- separate(focusdb, Registration_date, "/" ,
                     into = c("Registration_date","Month") )

# Changing NA to january in month variable
focusdb$Month[is.na(focusdb$Month)] <- 1

# Formatting year and month as number
focusdb <- mutate(focusdb, 
                  Registration_date = as.numeric(gsub("[^0-9]","",Registration_date ) ),
                   Month = round(as.numeric(gsub("[^0-9]","",Month ))/12,3))

# Creating the date of registration with decimals
focusdb <- mutate(focusdb, Registration = Registration_date + Month)

# Calculating age of cars by deducting their reg.date from 2020/10 (2020.833)
focusdb <- mutate(focusdb, Age = 2020.833-focusdb$Registration)

# Removing month and registration in decimals variables
focusdb <- select ( focusdb, -c(Month, Registration) )

# Reordering the columns for the final table 
focusdb <- focusdb [, c(1,3,4,5,15,7,9,12,11,10,13,6,8,14,2)]

# Writing out the cleaned database to a csv
data_out <- "C:/Users/Attila/Documents/CEU/Coding_in_R/Task_3_ford_focus_prices/Data/Clean/"
write_excel_csv(focusdb , paste0(data_out,"ford_focus.csv"))

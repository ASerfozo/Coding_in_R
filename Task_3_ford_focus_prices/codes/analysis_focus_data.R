##########################################
##             Data Analysis 2          ##
##              Term Project            ##
##             Attila Serfozo           ##
##                Modelling             ##
##########################################

rm(list=ls())
# loading packages
library(tidyverse)
library(ggthemes)
library(estimatr)

library(AER)
require(scales)
library(lspline)
library(texreg)


# Import raw data
data_in <- "https://raw.githubusercontent.com/ASerfozo/Coding_in_R/main/Task_3_ford_focus_prices/data/Clean/ford_focus.csv"
focusdb <- read_csv(data_in)

# Convert Price to milion HUF
focusdb <- mutate(focusdb, Price_mHUF = round(Price_HUF / 1000000, 2) )
focusdb <- select ( focusdb , -Price_HUF)

# Quick check on all HISTOGRAMS
focusdb %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_bw()+
  labs(x="Variable", y="Absolute frequency", title="Distribution of variables")

summary( focusdb )

# Filter out extreme values, these are Ford Focus RS cars, almost race cars for race tracks, their pricind is always special
price_extreme_values <- focusdb
price_extreme_values <- filter(price_extreme_values, Performance_HP>310)
price_extreme_values <- select(price_extreme_values, c(ID, Name, Price_mHUF, Registration_date, Kilometers, Performance_HP))
price_extreme_values <- mutate(price_extreme_values, Name=substr(Name,1,26))
focusdb <- filter(focusdb, focusdb$Performance_HP<310)

# Create dummy variables from fuel type and transmission
focusdb <- focusdb %>% mutate(Fuel_type_d=Fuel_type,
                              Transmission_d=Transmission)
focusdb$Fuel_type_d <- gsub("Benzin", "0", focusdb$Fuel_type_d)
focusdb$Fuel_type_d <- gsub("Dízel", "1", focusdb$Fuel_type_d)
focusdb$Fuel_type_d <- as.numeric(focusdb$Fuel_type_d)

focusdb$Transmission_d <- gsub("Manuális", "0", focusdb$Transmission_d)
focusdb$Transmission_d <- gsub("Automata", "1", focusdb$Transmission_d)
focusdb$Transmission_d <- as.numeric(focusdb$Transmission_d)

####
# Checking scatterplots
check_sp <- function(x_var){
  ggplot( focusdb , aes(x = x_var, y = Price_mHUF)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Averaged values of prices in milion") 
}

# Age of cars
check_sp(focusdb$Age)
check_sp(log(focusdb$Age))
# There is a good correlation
# Maybe taking the log helps

# level-level 0.69
# level-log 0.763
# log-level 0.722
# log-log 0.7

# Kilometers ran
check_sp(focusdb$Kilometers)
# It is correlated, taking the log of Price helps

# level-level 0.545
# level-log 0.474
# log-level 0.6
# log-log 0.388

# Performance in horse power
check_sp(focusdb$Performance_HP)
# Around 110 HP the the pattern maybe changes
# test it with P.L.S

# level-level 0.22
# level-log 0.245
# log-level 0.22
# log-log 0.265

# Fuel type (0 if petrol, 1 if diesel)
check_sp(focusdb$Fuel_type_d)
# seems like there is no difference between 0 and 1 

# level-level 0.008
# log-level 0.02

# Transmission type (0 if manual 1 if automatic)
check_sp(focusdb$Transmission_d)
# seems like uncorrelated, not too many automatic cars in the data unfortunately

# level-level 0.0546
# log-level 0.0545


####
# Comparing explanatory variables 
#
# Check the correlations
#
numeric_focusdb <- select(focusdb,c(Price_mHUF, Age, Kilometers, Performance_HP, Fuel_type_d, Transmission_d,))
cT <- cor(numeric_focusdb , use = "complete.obs")

cor(focusdb$Price_mHUF,focusdb$Condition)

# Check for highly correlated values:
sum( abs(cT) >= 0.8 & cT != 1 ) / 2
# Find the correlations which are higher than 0.8
id_cr <- which( abs(cT) >= 0.8 & cT != 1 )
pair_names <- expand.grid( variable.names(numeric_focusdb) , variable.names(numeric_focusdb) )
# Get the pairs:
high_corr <- pair_names[ id_cr , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr ] )
high_corr

# Results:
#   - there is a strong correlation between price and age
#       a) possible outcomes, 
#       b) not inteded to include in the main regression
# Remove the un-needed variables
rm( numeric_df, id_cr, pair_names )


#####
# 6) Modelling
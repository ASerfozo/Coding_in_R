##########################################
##             Data Analysis 2          ##
##              Term Project            ##
##             Attila Serfozo           ##
##                Modelling             ##
##########################################

rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(estimatr)   # Estimate robust SE
library(lspline)    # Estimate piecewise linear splines
library(segmented)  # For automated knots in PLS
library(texreg)     # Compare models with robust SE, export results to html
library(ggthemes)   # For ggplot themes



# Data import and some data transformation --------------------------------


# Import cleaned dataset
data_in <- "https://raw.githubusercontent.com/ASerfozo/Coding_in_R/main/Task_3_ford_focus_prices/data/Clean/ford_focus.csv"
focusdb <- read_csv(data_in)

# Scale Price to million HUF
focusdb <- mutate(focusdb, Price_mHUF = round(Price_HUF / 1000000, 2) )
focusdb <- select ( focusdb , -Price_HUF)

# Create dummy variables from fuel type and transmission
focusdb <- focusdb %>% mutate(Fuel_type_d=Fuel_type,
                              Transmission_d=Transmission)
focusdb$Fuel_type_d <- gsub("Benzin", "0", focusdb$Fuel_type_d)
focusdb$Fuel_type_d <- gsub("Dízel", "1", focusdb$Fuel_type_d)
focusdb$Fuel_type_d <- as.numeric(focusdb$Fuel_type_d)

focusdb$Transmission_d <- gsub("Manuális", "0", focusdb$Transmission_d)
focusdb$Transmission_d <- gsub("Automata", "1", focusdb$Transmission_d)
focusdb$Transmission_d <- as.numeric(focusdb$Transmission_d)



# Descriptives ------------------------------------------------------------


# Quick check on all HISTOGRAMS
focusdb %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_economist_white()+
  labs(x="Variable", y="Absolute frequency", title="Distribution of variables")

summary( focusdb )

# Check main parameters
ggplot( focusdb , aes(x = Price_mHUF)) +
  geom_histogram(binwidth=0.5,fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth=0.5,
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5)+
  labs(y="Absolute Frequency", x = "Distribution of Price in milion HUF", title="Distribution of Price of cars in milion HUF")+
  theme_economist_white()

ggplot( focusdb , aes(x = Age)) +
  geom_histogram(binwidth=0.5,fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth=0.5,
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5)+
  labs(y="Absolute Frequency", x = "Distribution of Age", title="Distribution of Age of cars in years")+
  theme_economist_white()

ggplot( focusdb , aes(x = Kilometers/1000)) +
  geom_histogram( binwidth=20, fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth=20,
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5)+
  labs(y="Absolute Frequency", x = "Distribution of Kilometers ran", title="Distribution of kilometers ran")+
  theme_economist_white()

ggplot( focusdb , aes(x = Performance_HP)) +
  geom_histogram(binwidth=10,fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth=10,
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.5)+
  labs(y="Absolute Frequency", x = "Distribution of Performance in Horsepower", title="Distribution of power of cars in horsepower")+
  theme_economist_white()

ggplot( focusdb , aes(x = Fuel_type_d)) +
  geom_histogram(binwidth=0.5,fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth = 0.5, 
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.4)+
  labs(y="Absolute Frequency", x = "Distribution of Fuel Type", title="Distribution of fuel type of cars")+
  theme_economist_white()

ggplot( focusdb , aes(x = Transmission_d)) +
  geom_histogram(binwidth=0.5,fill='navyblue', col="black", alpha=0.7) +
  stat_bin(geom = "text", binwidth = 0.5, 
           aes(label = ifelse(..count.. > 0, ..count.., "")), vjust = -0.4)+
  labs(y="Absolute Frequency", x = "Distribution of Transmission Type", title="Distribution of transmission type of cars")+
  theme_economist_white()



# Extreme values ----------------------------------------------------------


# These are Ford Focus RS cars, almost race cars for race tracks, their pricing is always special
price_extreme_values <- focusdb
filter(price_extreme_values, Price_mHUF > 8)

price_extreme_values <- filter(price_extreme_values, Performance_HP>310)
price_extreme_values <- select(price_extreme_values, c(ID, Name, Price_mHUF, Registration_date, Kilometers, Performance_HP))
price_extreme_values <- mutate(price_extreme_values, Name=substr(Name,1,26))
focusdb <- filter(focusdb, focusdb$Performance_HP<310)

rm(price_extreme_values)



# Check pattern of association --------------------------------------------


check_sp <- function(x_var){
  ggplot( focusdb , aes(x = x_var, y = Price_mHUF)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Averaged values of prices in milion")+
    theme_economist_white()
}

# Age of cars
check_sp(focusdb$Age)
check_sp(log(focusdb$Age))
# There is a good correlation
# test with P.L.S
# Maybe taking the log og Age helps

# Kilometers ran
check_sp(focusdb$Kilometers)

ggplot( focusdb , aes(x = Kilometers, y = log(Price_mHUF))) +
  geom_point() +
  geom_smooth(method="loess" , formula = y ~ x )+
  labs(y = "Averaged values of prices in milion")+
  theme_economist_white()
# It is correlated, taking the log of Price helps maybe

# Performance in horse power
check_sp(focusdb$Performance_HP)
# Around 116 HP the the pattern maybe changes
# test it with P.L.S

# Creating a dummy variable from HP helps better correlation
# Performance is 1 if HP is larger than 116
focusdb <- focusdb %>% mutate( Performance_HP_d = 1*(Performance_HP>116))
check_sp(focusdb$Performance_HP_d)
# there is definitely a difference in 0 and 1

# Fuel type (0 if petrol, 1 if diesel)
check_sp(focusdb$Fuel_type_d)
# seems like there is no significant difference between 0 and 1

# Transmission type (0 if manual 1 if automatic)
check_sp(focusdb$Transmission_d)
# seems uncorrelated, unfortunately not too many automatic cars in the data unfortunately

# Taking the log of price or age do not seem to improve the mode the fit significantly
# so I will keep all variables on level to make the interpretation easier and the model more simple



# Comparing explanatory variables -----------------------------------------


numeric_focusdb <- keep( focusdb , is.numeric )
cT_all <- cor(numeric_focusdb , use = "complete.obs")

# Check for highly correlated values:
sum( abs(cT_all) >= 0.8 & cT_all != 1 ) / 2

# Find the correlations which are higher than 0.8
id_cr <- which( abs(cT_all) >= 0.8 & abs(cT_all) != 1 )
pair_names <- expand.grid( variable.names(numeric_focusdb) , variable.names(numeric_focusdb) )
# Get the pairs:
high_corr_all <- pair_names[ id_cr , ]
high_corr_all <- mutate( high_corr_all , corr_val = cT_all[ id_cr ] )
high_corr_all

rm(numeric_focusdb, id_cr, pair_names)

# Results: registration date and Age are the same, so we drop registration date from the analysis as Age is more precise
# Performance in HP and in kW are the same. We keep horsepower in the model. The Perf.HP dummy has a much better correlation with price
# There is a high correlation between price and age.
# We drop number of pictures it has no significant correlation with price.

####
# Check the correlations only for the main variables

mv_focusdb <- select(focusdb,c(Price_mHUF, Age, Kilometers, Performance_HP_d, Fuel_type_d, Transmission_d,))
cT <- cor(mv_focusdb , use = "complete.obs")

## Check for highly correlated values:
sum( abs(cT) >= 0.8 & cT != 1 ) / 2

# Find the correlations which are higher than 0.8
id_cr_h <- which( abs(cT) >= 0.8 & abs(cT) != 1 )
pair_names_h <- expand.grid( variable.names(mv_focusdb) , variable.names(mv_focusdb) )
# Get the pairs:
high_corr <- pair_names_h[ id_cr_h , ]
high_corr <- mutate( high_corr , corr_val = cT[ id_cr_h ] )
high_corr

## Check for moderately correlated values:
sum( abs(cT) >= 0.6 & cT != 1 ) / 2

# Find the correlations which are higher than 0.6
id_cr_m <- which( abs(cT) >= 0.6 & abs(cT) < 0.8 )
pair_names_m <- expand.grid( variable.names(mv_focusdb) , variable.names(mv_focusdb) )
# Get the pairs:
medium_corr <- pair_names_m[ id_cr_m , ]
medium_corr <- mutate( medium_corr , corr_val = cT[ id_cr_m ] )
medium_corr

# Results:
#   - there is a strong correlation between price and age
#   - there is a good correlation between price and kilometers
#   - there is a good correlation between price and performance horsepower dummy
#   - there is correlation between kilometers and age of course, but we will keep this two variables in the model as the correlation is not strong
#   - there is no significant correlation between price and fuel type and transmission type as they are meaningful variables in case of a car

rm( mv_focusdb, id_cr_h, pair_names_h, id_cr_m, pair_names_m )



# Modelling ---------------------------------------------------------------


# main variable of interest is age

# main regression: Price_mHUF = b0 + b1 * Age
#   reg1: NO controls, simple linear
#   reg2: NO controls, use piecewise linear spline(P.L.S) with a knot at 3 years old cars
#         also check for optimal knot point
# Use reg2 and control for:
#   reg3: performance_hp dummy
#   reg4: reg3 + Kilometers
#   reg5: reg4 + salary with P.L.S, knots at 35 and 40, exptot, log of income and scratio

#### reg1: no control, simple linear regression
reg1 <- lm_robust(Price_mHUF ~ Age  , data = focusdb )
summary( reg1 )
# R-squared:0.6902
# Coefficient estimate is -0.42

#### reg2: no controls, use piecewise linear spline(P.L.S) with a knot at 3 years

# find automatically the optimal knot point
library(segmented)
reg1_lm <- lm( Price_mHUF ~  Age, data = focusdb )
fit_seg <- segmented( reg1_lm , seg.Z = ~Age, psi = list( Age=4 ) )
summary(fit_seg)
# 3.166 is the estimated knot for Age of cars

reg2 <- lm_robust(Price_mHUF ~  lspline(Age, 3), data = focusdb )
summary( reg2 )
# R-squared is 0.7764
# coefficient estimate is -1.49 until 3 years
#                         -0.30 after 3 years

# Use interaction for Age, dummy is Age > 3
reg21 <- lm_robust(Price_mHUF ~ Age + (Age > 3) + Age*(Age > 3), data = focusdb )
summary( reg21 )

###
# Models with controls:
#
# reg3: control for Performance_HP and dummy. 

reg3 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                   + Performance_HP,
                   data = focusdb )
summary( reg3 )
# R-squared: 0.8659
# coefficient estimate is 0.0154


# Check lspline for performance
reg31 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                    + lspline(Performance_HP,116),
                    data = focusdb )
summary( reg31 )
# Does not change the model, keep it simple

reg32 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                   + Performance_HP_d,
                   data = focusdb )
summary( reg32 )
# R-squared is worse 0.8225
# but big difference in coefficient estimate 0.73

# Results: interestingly even though performance dummy had a better correlation with price, including
# the Performance_HP number results a better model R-squared, however coefficient is much larger

# reg4: reg3 + Kilometers
reg4 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                   + Performance_HP
                   + Kilometers,
                   data = focusdb )

summary( reg4 )
# R-squared 0.8988

# reg5: reg4 + transmission dummy

reg5 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                   + Performance_HP
                   + Kilometers
                   + Transmission_d,
                   data = focusdb )
summary( reg5 )

# reg6: reg5 + fuel type dummy

reg6 <- lm_robust( Price_mHUF ~  lspline(Age, 3) 
                   + Performance_HP
                   + Kilometers
                   + Transmission_d
                   + Fuel_type_d,
                   data = focusdb )
summary( reg6 )
# Fuel type does not add to the model, what's more it even decreases a little our R-squared value
# So it seems like it does not have a significant effect in case of Ford Focus Hatchbacks whether 
# the car runs with petrol or diesel, in addition including it soaks up the effect of another variable.

###
# Summarize findings:
data_out <- "C:/Users/Attila/Documents/CEU/Coding_in_R/Task_3_ford_focus_prices/out/"
htmlreg( list(reg1 , reg2 , reg3 , reg4 , reg5, reg6),
         type = 'html',
         custom.header = list("Average prices for Ford Focuses"=1:6),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
         custom.coef.names = c("Intercept","Age","Age (<3)","Age (>=3)",
                               "Performance_HP","Kilometers","Transmission_d","Fuel_type_d"),
         omit.coef = "Intercept|Performance_HP|Kilometers|Transmission_d|Fuel_type_d",
         file = paste0( data_out ,'ford_focus_models.html'), include.ci = FALSE,
         single.row = FALSE, siunitx = TRUE,
         custom.gof.rows = list( "Performance HP" = c("NO","NO","YES","YES","YES","YES"),
                                 Kilometers = c("NO","NO","NO","YES","YES","YES"),
                                 Transmission = c("NO","NO","NO","NO","YES","YES"),
                                 "Fuel Type" = c("NO","NO","NO","NO","NO","YES")))
# Results:
# (1) If we only use a simple linear regression, the Age coefficient is -0.42 which is significant at any levels
# (2) PLS the Age is significant below 3 and above 3 years as well at any significant level
#     - In case of cars less than 3 years old, cars 1 year older are in average  1.5 million HUF less expensive cars
#     - On the other in case of hand cars more than 3 years old, cars 1 year older are in average 0.3 million HUF less expensive



# Prediction --------------------------------------------------------------


####
# y_hat-y plot - reg5
focusdb <- mutate( focusdb , y_hat = predict( reg4 , focusdb ) )

ggplot( data = focusdb ) +
  geom_point (aes( x = y_hat , y = Price_mHUF ) ,  color="red")+
  geom_line( aes( x = Price_mHUF , y = Price_mHUF ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted car prices", y = "Actual car prices")+
  theme_economist_white()

# Get BIC and AIC measures for the model:
#
# Unfortunately lm_robust does not have this... 
# You can use simple lm (remember, in this case SEs are not important!)

# Does adding Fuel Type and Transmission Type increase the prediction?
reg4_lm <- lm( Price_mHUF ~  lspline(Age, 3) 
               + Performance_HP
               + Kilometers, data = subset(focusdb,complete.cases(focusdb) ) )

reg5_lm <- lm( Price_mHUF ~  lspline(Age, 3) 
               + Performance_HP
               + Kilometers
               + Transmission_d
               + Fuel_type_d, data = subset(focusdb,complete.cases(focusdb) ) )

BIC(reg4_lm,reg5_lm)
AIC(reg4_lm,reg5_lm)
# In case of BIC the reg4 is the better model
# In case of AIC the reg5 is the better
# So in this case both model is equally good, there is no big difference

####
# Predict the price of my Ford Focus car with reg4
#
# Price = b0 + b1 * Age(Age<3) + b2 * Age(Age>=3) + b3 * Performanc_HP + b4 * Kilometers
b0 <- 6.411
b1 <- -1.260
b2 <- -0.2107
b3 <- 0.01346
b4 <- -5.143*10^(-6)

# Properties of my Ford Focus car
# Reg.Date: 2011/2 -> Age 8.67; Performance_HP: 125; Kilometers: 97000
My_Price <- b0 + b1 * 3 + b2 * 5.67 + b3 * 125 + b4 * 97000
# Price of my car according to the model is 2.62 mHUF



# Residual analysis -------------------------------------------------------


# Get the predicted y values from the model
focusdb$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
focusdb$reg4_res <- focusdb$Price_mHUF - focusdb$reg4_y_pred 

# Find countries with largest negative errors
focusdb %>% top_n( -5 , reg4_res ) %>% 
  select( ID, Name, Price_mHUF, reg4_y_pred , reg4_res, Registration_date, Kilometers, Performance_HP )

# Find countries with largest positive errors
focusdb %>% top_n( 5 , reg4_res ) %>% 
  select( ID, Name, Price_mHUF, reg4_y_pred , reg4_res, Age, Kilometers, Performance_HP )



# External Validity -------------------------------------------------------

# Check model for Honda Civic Hatchback max 10 years old

####
# Import cleaned dataset
data_in_civic <- "https://raw.githubusercontent.com/ASerfozo/Coding_in_R/main/Task_3_ford_focus_prices/data/Clean/honda_civic.csv"
civicdb <- read_csv(data_in_civic)

####
# Scale Price to million HUF
civicdb <- mutate(civicdb, Price_mHUF = round(Price_HUF / 1000000, 2) )
civicdb <- select ( civicdb , -Price_HUF)

####
# Create dummy variables from fuel type and transmission
civicdb <- civicdb %>% mutate(Fuel_type_d=Fuel_type,
                              Transmission_d=Transmission)
civicdb$Fuel_type_d <- gsub("Benzin", "0", civicdb$Fuel_type_d)
civicdb$Fuel_type_d <- gsub("Dízel", "1", civicdb$Fuel_type_d)
civicdb$Fuel_type_d <- as.numeric(civicdb$Fuel_type_d)

civicdb$Transmission_d <- gsub("Manuális", "0", civicdb$Transmission_d)
civicdb$Transmission_d <- gsub("Automata", "1", civicdb$Transmission_d)
civicdb$Transmission_d <- as.numeric(civicdb$Transmission_d)

####
# Extreme values similarly to Focuses, here Honda Civic Type R cars are the race cars
filter(civicdb, Price_mHUF > 8)
civicdb <- filter(civicdb, civicdb$Performance_HP<310)


####
# Check scatterplots
check_sp <- function(x_var){
  ggplot( civicdb , aes(x = x_var, y = Price_mHUF)) +
    geom_point() +
    geom_smooth(method="loess" , formula = y ~ x )+
    labs(y = "Averaged values of prices in milion")+
    theme_economist_white()
}

check_sp(civicdb$Age)
# Age is similar, but larger Age values so knot around 5 to choose for spline
check_sp(civicdb$Kilometers)
# Kilometers look like the same
check_sp(civicdb$Performance_HP)
check_sp(civicdb$Fuel_type_d)
check_sp(civicdb$Transmission_d)

####
# Check models
reg1_e <- lm_robust(Price_mHUF ~ Age  , data = civicdb )
summary( reg1_e )

reg2_e <- lm_robust(Price_mHUF ~  lspline(Age, 5), data = civicdb )
summary( reg2_e )

reg3_e <- lm_robust( Price_mHUF ~  lspline(Age, 5) 
                   + Performance_HP,
                   data = civicdb )
summary( reg3_e )

reg4_e <- lm_robust( Price_mHUF ~  lspline(Age, 5) 
                   + Performance_HP
                   + Kilometers,
                   data = civicdb )

summary( reg4_e )

reg5_e <- lm_robust( Price_mHUF ~  lspline(Age, 5) 
                   + Performance_HP
                   + Kilometers
                   + Transmission_d,
                   data = civicdb )
summary( reg5_e )

reg6_e <- lm_robust( Price_mHUF ~  lspline(Age, 5) 
                   + Performance_HP
                   + Kilometers
                   + Transmission_d
                   + Fuel_type_d,
                   data = civicdb )
summary( reg6_e )

####
# Summarize findings:
htmlreg( list(reg1_e , reg2_e , reg3_e , reg4_e , reg5_e, reg6_e),
         type = 'html',
         custom.header = list("Average prices for Honda Civics"=1:6),
         custom.model.names = c("(1)","(2)","(3)","(4)","(5)","(6)"),
         custom.coef.names = c("Intercept","Age","Age (<5)","Age (>=5)",
                               "Performance_HP","Kilometers","Transmission_d","Fuel_type_d"),
         omit.coef = "Intercept|Performance_HP|Kilometers|Transmission_d|Fuel_type_d",
         file = paste0( data_out ,'honda_civic_models.html'), include.ci = FALSE,
         single.row = FALSE, siunitx = TRUE,
         custom.gof.rows = list( "Performance HP" = c("NO","NO","YES","YES","YES","YES"),
                                 Kilometers = c("NO","NO","NO","YES","YES","YES"),
                                 Transmission = c("NO","NO","NO","NO","YES","YES"),
                                 "Fuel Type" = c("NO","NO","NO","NO","NO","YES")))

# Results:
# similarly to the Ford Focus database the model has a strong estimation for 
# car prices with an R-squared of 0.9. Also the regression 4 was the best choice
# in case of the Honda Civic as well.

####
# Show prediction for Honda Civic
civicdb <- mutate( civicdb , y_hat = predict( reg4_e , civicdb ) )

ggplot( data = civicdb ) +
  geom_point (aes( x = y_hat , y = Price_mHUF ) ,  color="red")+
  geom_line( aes( x = Price_mHUF , y = Price_mHUF ) , color = "navyblue" , size = 1.5 )+
  labs( x = "Predicted car prices", y = "Actual car prices")+
  theme_economist_white()



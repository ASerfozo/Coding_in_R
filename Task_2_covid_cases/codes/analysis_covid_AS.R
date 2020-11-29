########################
##  Attila Serfozo    ##
## Assignment for DA2 ##
##  and for Coding    ##
##                    ##
##   NO. 3            ##
## Data Analysis      ##
########################


# Loading the packages
rm(list=ls())
library(tidyverse)
require(scales)
library(lspline)
library(estimatr)
library(texreg)
library(ggthemes)

######
# Load the data
my_path <- "https://raw.githubusercontent.com/ASerfozo/Coding_in_R/main/Task_2_covid_cases/data/"
df <- read_csv(paste0(my_path,'clean/covid_pop_10_17_2020_clean.csv'))

## The parameters
# X-variable: number of registered case
# Y-variable: Number of registered death

## Check the variables
df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram()+
  theme_wsj() + 
  scale_fill_wsj()

summary(df)

## Distribution of X and Y variables
# Basically the distributions look like an exponential distribution
# Using the log of the variables show a better picture
df <- df %>% mutate(ln_death = log(death))
df <- df %>% mutate(ln_confirmed = log(confirmed))

df %>% ggplot() +
  geom_histogram(aes(x = ln_death), bins = 50)
  labs(x = "Number of deaths in ln scales")
  
df %>% ggplot() +  
  geom_histogram(aes(x = ln_confirmed), bins = 50) +
  labs(x = "Number of confirmed cases in ln scales")

summary(df)


######
# Check different possible ln transformations for death - confirmed cases
# level-level vs level-log vs log-level vs log-log
#
#
# 1. level-level: death - confirmed
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases in thousands",y = "Number of death in thousands") 

# 2. level-log: death - ln confirmed
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases in thousands",y = "Number of death in thousands (ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 3. log-level: ln death - confirmed
ggplot( df , aes(x = confirmed, y = death)) +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases in thousands (ln scale)",y = "Number of death in thousands") +
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

# 4. log-log: ln death - ln confirmed
ggplot( df , aes(x = confirmed, y = death ))  +
  geom_point() +
  geom_smooth(method="loess")+
  labs(x = "Number of confirmed cases in thousands (ln scale)",y = "Number of death in thousands (ln scale)") +
  scale_x_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )+
  scale_y_continuous( trans = log_trans(),  breaks = c(1,2,5,10,20,50,100,200,500,1000,10000) )

##
# Conclusions:
# Taking the log-log of the death and confirmed variables looks to have the best fit
# As both variables have an exponential distribution if level, taking the log of both is better


######
# Models
# simple linear regression
#   reg1: ln_death = alpha + beta * ln_confirmed
# quadratic regression
#   reg2: ln_death = alpha + beta_1 * ln_confirmed + beta_2 * ln_confirmed^2
# piecewise linear spline regression
#   reg3: ln_death = alpha_1 + beta_1 * ln_confirmed + alpha_2 + beta_2 * ln_confirmed
# weighted linear regression, using population weights
#   reg4: ln_death = alpha + beta * ln_confirmed, weight: population
#
#

#### 1. Simple linear regression
reg1 <- lm_robust( ln_death ~ ln_confirmed , data = df , se_type = "HC2" )
summary( reg1 ) 
# --> R-squared 0.8861

ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( method = lm , color = 'red' )

#### 2. Quadratic regression
df <- df %>% mutate( ln_confirmed_sq = ln_confirmed^2)
reg2 <- lm_robust( ln_death ~ ln_confirmed + ln_confirmed_sq , data = df )
summary( reg2 )
# --> R-squared 0.8898

ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ poly(x,2) , method = lm , color = 'red' )

#### 3. Piecewise linear spline
# define the cutoff
cutoff <- 30
cutoff_ln <- log(cutoff)

reg3 <- lm_robust(ln_death ~ lspline( ln_confirmed , cutoff_ln ), data = df )
summary( reg3 )
# --> R-squared: 0.8912

ggplot( data = df, aes( x = ln_confirmed, y = ln_death ) ) + 
  geom_point( color='blue') +
  geom_smooth( formula = y ~ lspline(x,cutoff_ln) , method = lm , color = 'red' )

#### 4.Weighted linear regression, using population weights
reg4 <- lm_robust(ln_death ~ ln_confirmed, data = df , weights = population)
summary( reg4 )
# R-squared 0.9283

ggplot(data = df, aes(x = ln_confirmed, y = ln_death)) +
  geom_point(data = df, aes(size=population),  color = 'blue', shape = 16, alpha = 0.6,  show.legend=F) +
  geom_smooth(aes(weight = population), method = "lm", color='red')+
  scale_size(range = c(1, 10))+
  labs(x = "ln(confirmed cases in thousands)",y = "ln(death cases in thousands)")+
  annotate("text", x = 8.8, y = 4, label = "USA", size=5)+
  annotate("text", x = 3.5, y = 2.5, label = "China", size=5)

### Final model arguing 
#
# Based on model comparison my chosen model is reg4 - linear regression with population weights
#   Substantive: - log-log interpretation works properly for countries
#                - magnitude of coefficients are meaningful
#   Statistical: - Highest R-square with 0.9283
#                

### Write out results to a table
my_path <- "C:/Users/Attila/Documents/CEU/Coding_in_R/Task_2_covid_cases/"
data_out <- my_path
htmlreg( list(reg1 , reg2 , reg3 , reg4),
         type = 'html',
         custom.model.names = c("ln_death/ln_confirmed - simple linear",
                                "ln_death/ln_confirmed - quadratic",
                                "ln_death/ln_confirmed - PLS",
                                "ln_death/ln_confirmed - linear, weighted"),
         caption = "Modelling the registered confirmed and death cases of countries due to COVID-19",
         file = paste0( data_out ,'out/model_comparison.html'), include.ci = FALSE)


##### Hypothesis testing on  coefficient(s)

# H0: Coefficient is equal to 0:
# Implemented by default...
summary( reg4 )

# We are rejecting the 0 hypothesis, that ß coefficent on log confirmed cases is equal to zero as the t-value (15.24) 
# is far away from 2.6 (99% CI). 
# The p-value is also very close to 0 showing that there is an extremely low probability for the value be larger than 't'.
# 

##### Residual analysis

# Get the predicted y values from the model
df$reg4_y_pred <- reg4$fitted.values
# Calculate the errors of the model
df$reg4_res <- df$ln_death - df$reg4_y_pred 

# Find countries with largest negative errors
df %>% top_n( -5 , reg4_res ) %>% 
  select( country , ln_death , reg4_y_pred , reg4_res )

# Find countries with largest positive errors
df %>% top_n( 5 , reg4_res ) %>% 
  select( country , ln_death , reg4_y_pred , reg4_res )

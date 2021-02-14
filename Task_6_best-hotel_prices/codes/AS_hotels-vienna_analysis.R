################################
##        Serfőző Attila      ##
##        Data Analysis 3     ##
##         Assignment 3       ##   
##          Prediction        ##
################################

# Set up environment ------------------------------------------------------

rm(list=ls())


# Import libraries
library(tidyverse)
library(scales)
library(xtable)
library(cowplot)
library(glmnet)
library(caret)
library(rattle)

dir <- "D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/Assignments/Assignment_3/"

# load theme and functions
source(paste0(dir,"codes/theme_bg.R"))
source(paste0(dir,"codes/da_helper_functions.R"))

data_in <- paste0(dir,"data/clean/")
data_out <- paste0(dir,"out/")

# load vienna
vienna <- readRDS(paste0(data_in,"hotels-vienna.rds"))

vienna %>%
  group_by(n_rating) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))


# Filter to scope of analysis ---------------------------------------------
describe(vienna$price)
describe(vienna$f_stars)
# 3-4 stars, Vienna actual, without  extreme values (price above 600 USD)
vienna <- vienna %>% filter(f_accommodation_type=="Hotel") %>%
  filter(f_city_actual=="Vienna") %>%
  filter(f_stars==3 | f_stars==3.5 | f_stars==4) %>% 
  filter(price<=300)


# Exploratory data analysis -----------------------------------------------

# Look at missing values
to_filter <- sapply(vienna, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# Price is skewed with a long right tail
# Logarithmic transformation is preferable
plot_price <- ggplot(data = vienna, aes (x = price)) +
  geom_histogram(binwidth = 10, color = "black", fill = "seagreen4")+
  labs(title="Distribution of price", x = "Price (US dollars)", y = "Frequency") +
  theme_bw() 
plot_price

vienna$lnprice <- log(vienna$price)

# lnprice -> closer to a normal distribution
plot_lnprice <- ggplot(data = vienna, aes (x = lnprice)) +
  geom_histogram(color = "black", fill = "seagreen4")+
  labs(title="Distribution of log price", x = "Price (US dollars)", y = "Frequency") +
  theme_bw() 
plot_lnprice

# Distance is better with a logarithmic transformation
plot_distance <- ggplot(data = vienna, aes (x = n_distance)) +
  geom_histogram(binwidth = 0.3, color = "black", fill = "seagreen4")+
  labs(title="Distribution of distance", x = "Distance to city center (miles)", y = "Frequency") +
  expand_limits(x = 0.01, y = 0.01) +
  # scale_x_continuous(expand = c(0.01,0.01), limits = c(0,14), breaks = seq(0, 14, by = 2)) +
  # scale_y_continuous(expand = c(0.00,0.00), limits = c(0,61), breaks = seq(0, 60, by = 10)) +
  theme_bw() 
plot_distance

vienna$n_distance_log <- log(vienna$n_distance)

plot_lndistance <- ggplot(data = vienna, aes (x = n_distance_log)) +
  geom_histogram(binwidth = 0.3, color = "black", fill = "seagreen4")+
  labs(title="Distribution of log distance", x = "Distance to city center (miles)", y = "Frequency") +
  # expand_limits(x = 0.01, y = 0.01) +
  # scale_x_continuous(expand = c(0.01,0.01), limits = c(0,14), breaks = seq(0, 14, by = 2)) +
  # scale_y_continuous(expand = c(0.00,0.00), limits = c(0,61), breaks = seq(0, 60, by = 10)) +
  theme_bw() 
plot_lndistance


# How does average price changes across variables
# number of stars
boxplot_stars <- ggplot(data = vienna, aes(x = f_stars, y = price)) +
  stat_boxplot(aes(group = f_stars), geom = "errorbar", width = 0.3,
               color = "black", size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_stars),
               color = "black", fill = c("seagreen2","seagreen3","seagreen4"),
               size = 0.5, width = 0.6, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(title="Distribution of price by stars",x = "Number of stars",y = "Price (US dollars)")+
  theme_bw()
boxplot_stars

# tripadvisor rating
boxplot_ratingta <- ggplot(data = vienna, aes(x = f_ratingta, y = price)) +
  stat_boxplot(aes(group = f_ratingta), geom = "errorbar", width = 0.3,
               color = "black", size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_ratingta),
               color = "black", fill = c("seagreen2", "seagreen2", "seagreen3","seagreen3","seagreen4"),
               size = 0.5, width = 0.6, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(title="Distribution of price by Tripadvisor rating",x = "Rating",y = "Price (US dollars)")+
  theme_bw()
boxplot_ratingta

vienna %>%
  group_by(d_scarce_room) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

vienna %>%
  group_by(f_neighbourhood) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))


# Define models -----------------------------------------------------------

main_variables <- c("n_distance_log", "f_stars", "n_rating", "f_neighbourhood")
  
reviews_var <- c("n_rating_count", "f_ratingta", "n_ratingta_count")

other_var <- c("n_distance_alter","d_offer", "d_scarce_room", "f_offer_cat")

X1 <- c(main_variables)
X2 <- c(main_variables, reviews_var)
X3 <- c(main_variables, reviews_var, other_var)


# Modelling ---------------------------------------------------------------


# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


### OLS

# For main vars
set.seed(2021)
ols_model <- train(
  formula(paste0("lnprice ~", paste0(X1, collapse = " + "))),
  data = vienna,
  method = "lm",
  trControl = train_control
)

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))


# For main vars and review vars
set.seed(2021)
system.time({
  ols_model2 <- train(
    formula(paste0("lnprice ~", paste0(X2, collapse = " + "))),
    data = vienna,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs2 <-  ols_model2$finalModel$coefficients
ols_model_coeffs_df2 <- data.frame(
  "variable" = names(ols_model_coeffs2),
  "ols_coefficient" = ols_model_coeffs2
) %>%
  mutate(variable = gsub("`","",variable))

# For main vars and review vars and other vars
set.seed(2021)
system.time({
  ols_model3 <- train(
    formula(paste0("lnprice ~", paste0(X3, collapse = " + "))),
    data = vienna,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs3 <-  ols_model3$finalModel$coefficients
ols_model_coeffs_df3 <- data.frame(
  "variable" = names(ols_model_coeffs3),
  "ols_coefficient" = ols_model_coeffs3
) %>%
  mutate(variable = gsub("`","",variable))


### CART

set.seed(2021)
system.time({
  cart_model <- train(
    formula(paste0("lnprice ~", paste0(X3, collapse = " + "))),
    data = vienna,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
    
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")



### RANDOM FOREST

# set tuning
tune_grid <- expand.grid(
  .mtry = c(9),
  .splitrule = "variance",
  .min.node.size = c(10)
)


# simpler model with basic vars and review vars for model A (1)
set.seed(2021)
system.time({
  rf_model <- train(
    formula(paste0("lnprice ~", paste0(X3, collapse = " + "))),
    data = vienna,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model


# Compare models ----------------------------------------------------------

final_models <-
  list("OLS (main)" = ols_model,
       "OLS (main + reviews)" = ols_model2,
       "OLS (main + reviews + others)" = ols_model3,
       "CART" = cart_model,
       "Random forest" = rf_model)

results <- resamples(final_models) %>% summary()
results

# Model selection is carried out on this CV RMSE
# CART and Random forest (smaller model) are performing worse
result_1 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~Rsquared")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV Rsquared" = ".")
result_1

result_2 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")
result_2


# Actual versus predicted -------------------------------------------------

vienna <- vienna %>%
  mutate(predicted_price_ln = predict(rf_model, newdata = vienna))

vienna <- vienna %>%
  mutate(ln_res = lnprice - predicted_price_ln)

std <- sd(vienna$ln_res)

vienna <- vienna %>%
  mutate(predicted_price = round(exp(vienna$predicted_price_ln) * exp((std^2)/2),2))

# Actual versus predicted price

level_vs_pred <- ggplot(data = vienna) +
  geom_point(aes(y=price, x=predicted_price), color = "seagreen4", size = 1,
             shape = 16, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 250, yend =250), size=0.5, color="black", linetype=2) +
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 250)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 250), breaks=seq(0, 250, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 250), breaks=seq(0, 250, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bw() 
level_vs_pred


# Best 5 hotels
vienna <- vienna %>%
  mutate(price_res = round(price - predicted_price,4))

Best_hotels <- vienna %>% select(hotel_id, price, predicted_price, price_res, ln_res, n_distance, f_neighbourhood, f_stars, n_rating) %>%
  arrange(ln_res)

Best5_hotels <- head(Best_hotels,5)

# Best Deals
# IDs: 21912, 21975, 22104, 22032, 22407
Best5_hotels


# Best Deals in Book:
# IDs: 21912, 21975, 22344, 22080, 22184
Best5_hotels_book <- Best_hotels[Best_hotels$hotel_id %in% c(21912, 21975, 22344, 22080, 22184),]
Best5_hotels_book


vienna$bestdeals <- ifelse(vienna$ln_res %in% head(sort(vienna$ln_res),5),TRUE,FALSE)

bestdeals <- ggplot(data = vienna, aes(x = predicted_price, y = price)) +
  geom_point(aes(color=bestdeals,shape=bestdeals), size = 1.2, fill="black", alpha = 0.8, show.legend=F, na.rm = TRUE) + 
  geom_segment(aes(x = 0, y = 0, xend = 250, yend =250), size=0.8, color="seagreen4", linetype=2) +
  labs(x = "Predicted price in US dollars ",y = "Price in US dollars")+
  coord_cartesian(xlim = c(0, 250), ylim = c(0, 250)) +
  scale_colour_manual(name='',values=c("grey",'red')) +
  scale_shape_manual(name='',values=c(16,21)) +
  geom_segment(aes(x = 110, y = 55, xend = 140, yend = 110), arrow = arrow(length = unit(0.1, "cm")))+
  geom_segment(aes(x = 100, y = 55, xend = 95, yend = 70), arrow = arrow(length = unit(0.1, "cm")))+
  geom_segment(aes(x = 90, y = 50, xend = 70, yend = 50), arrow = arrow(length = unit(0.1, "cm")))+
  annotate("text", x = 110, y = 50, label = "Best deals", size=2.5)+
  theme_bw() +
  theme(axis.text.x=element_text(size=10)) +
  theme(axis.text.y=element_text(size=10)) +
  theme(axis.title.x=element_text(size=10)) +
  theme(axis.title.y=element_text(size=10)) 
bestdeals

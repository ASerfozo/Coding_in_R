################################
##        Serfőző Attila      ##
##        Data Analysis 3     ##
##         Assignment 1       ##   
##          Prediction        ##
################################


# Set up environment ------------------------------------------------------

# Set libraries
rm(list=ls())
library(tidyverse)
library(caret)
library(skimr) # for skimr data view
library(Hmisc) # for describe variable
library(grid) # for plot grid
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot) # for plot grid and boxplot
library(rattle)
library(ranger)
library(kableExtra)


# Set directory
dir<-"D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/DA3_R/Assignment 1"
setwd(dir)

data_in  <- paste0(dir,"/data/clean/")
data_out <- paste0(dir,"/out/")

options(digits = 3)
source("codes/theme_bg.R")
source("codes/da_helper_functions.R")

# Import data
data <-
  read_csv(paste0(data_in,"airbnb_rome_cleaned.csv")) %>%
  mutate_if(is.character, factor)


# Look at data -----------------------------------------------------------


glimpse(data)
skim(data)

# Where do we still have missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# drop variables with still too many missing values
to_drop <- c("usd_cleaning_fee", "p_host_response_rate", "d_reviews_per_month")
data <- data %>%
  select(-one_of(to_drop))

# Check price variable - we already excluded apartments with prices above 500 EUR as extreme values
Hmisc::describe(data$price)

# How does average price change across property type, room type, bed type, cancellation policy and neighbourhood
data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(f_bed_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(f_cancellation_policy) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

data %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))


## Histograms

# price -> skewed distribution with long right tail
ggplot(data=data, aes(x=price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 color = "black", fill = "indianred3") +
  coord_cartesian(xlim = c(0, 500)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,500), breaks = seq(0,500, 50)) +
  theme_bw() 


# lnprice -> closer to a normal distribution
ggplot(data=data, aes(x=ln_price)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.15,
                 color = "black", fill = "indianred3") +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bw() 


# Distribution of price by room type with boxplot
#   Entire apartments are much more expensive than private or shared rooms
ggplot(data = data, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = "black", size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = "black", fill = c("indianred2","indianred","indianred4"),
               size = 0.5, width = 0.6, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bw()


# Distribution of price by number of accommodates with boxplot
# Larger places with more people are more expensive and Apartments are more expensive than condominiums
ggplot(data, aes(x = factor(n_accommodates), y = price,
                 fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  scale_color_manual(name="",
                     values=c("indianred","indianred4")) +
  scale_fill_manual(name="",
                    values=c("indianred","indianred4")) +
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bw() +
  theme(legend.position = c(0.3,0.8)        )



# Define models -----------------------------------------------------------


# Basic Variables
basic_vars <- c(
  "n_accommodates", "n_beds", "n_days_since",
  "f_property_type","f_room_type", "f_bathroom", "f_cancellation_policy", "f_bed_type",
  "f_neighbourhood_cleansed")

# reviews
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")

#not use p_host_response_rate and usd_cleaning_fee due to missing values

# Dummy variables
amenities <-  grep("^d_.*", names(data), value = TRUE)


### Interactions

# Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type", "d_familykidfriendly", "Room type", "Family kid friendly")
p2 <- price_diff_by_variables2(data, "f_room_type", "f_property_type", "Room type", "Property type")
p3 <- price_diff_by_variables2(data, "f_room_type", "d_balcony", "Room type", "Balcony")
p4 <- price_diff_by_variables2(data, "f_room_type", "d_pool", "Room type", "Pool")
# Look up cancelation policy
p5 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_familykidfriendly", "Cancellation policy", "Family kid friendly")
p6 <- price_diff_by_variables2(data, "f_cancellation_policy", "d_tv", "Cancellation policy", "TV")
# Look up property type
p7 <- price_diff_by_variables2(data, "f_property_type", "d_cats", "Property type", "Cats")
p8 <- price_diff_by_variables2(data, "f_property_type", "d_dogs", "Property type", "Pool")

plots <- plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, nrow=4, ncol=2)
plots

rm(p1,p2,p3,p4,p5,p6,p7,p8)

# interactions for the LASSO
X1  <- c("n_accommodates*f_property_type",  "f_room_type*f_property_type",  "f_room_type*d_familykidfriendly",
         "d_airconditioning*f_property_type", "d_cats*f_property_type", "d_dogs*f_property_type",
         "f_room_type*d_balcony","f_room_type*d_pool")
# with boroughs
X2  <- c("f_property_type*f_neighbourhood_cleansed", "f_room_type*f_neighbourhood_cleansed",
         "n_accommodates*f_neighbourhood_cleansed" )


predictors_1 <- c(basic_vars)
predictors_2 <- c(basic_vars, reviews)
predictors_3 <- c(basic_vars, reviews, amenities)
predictors_E <- c(basic_vars, reviews, amenities, X1,X2)


# Separate hold-out set ---------------------------------------------------

set.seed(2021)

train_indices <- as.integer(createDataPartition(data$price, p = 0.7, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]


# Modelling ---------------------------------------------------------------


# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)


### OLS

# For basic vars
set.seed(2021)
system.time({
  ols_model <- train(
    formula(paste0("price ~", paste0(predictors_1, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs <-  ols_model$finalModel$coefficients
ols_model_coeffs_df <- data.frame(
  "variable" = names(ols_model_coeffs),
  "ols_coefficient" = ols_model_coeffs
) %>%
  mutate(variable = gsub("`","",variable))


# For basic vars and review vars
set.seed(2021)
system.time({
  ols_model2 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "lm",
    trControl = train_control
  )
})

ols_model_coeffs2 <-  ols_model$finalModel$coefficients
ols_model_coeffs_df2 <- data.frame(
  "variable" = names(ols_model_coeffs2),
  "ols_coefficient" = ols_model_coeffs2
) %>%
  mutate(variable = gsub("`","",variable))


### LASSO
# using extended model with interactions

set.seed(2021)
system.time({
  lasso_model <- train(
    formula(paste0("price ~", paste0(predictors_E, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    tuneGrid =  expand.grid("alpha" = 1, "lambda" = seq(0.01, 0.25, by = 0.01)),
    trControl = train_control
  )
})

lasso_coeffs <- coef(
  lasso_model$finalModel,
  lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(lasso_coefficient = `1`)  # the column has a name "1", to be renamed

lasso_coeffs_non_null <- lasso_coeffs[!lasso_coeffs$lasso_coefficient == 0,]
regression_coeffs <- merge(ols_model_coeffs_df2, lasso_coeffs_non_null, by = "variable", all=TRUE)

### CART

set.seed(2021)
system.time({
  cart_model <- train(
    formula(paste0("price ~", paste0(predictors_3, collapse = " + "))),
    data = data_train,
    method = "rpart",
    tuneLength = 10,
    trControl = train_control
    
  )
})

fancyRpartPlot(cart_model$finalModel, sub = "")


### RANDOM FOREST

# set tuning
tune_grid <- expand.grid(
  .mtry = c(5, 7, 9),
  .splitrule = "variance",
  .min.node.size = c(5, 10)
)


# simpler model with basic vars and review vars for model A (1)
set.seed(2021)
system.time({
  rf_model_1 <- train(
    formula(paste0("price ~", paste0(predictors_2, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})
rf_model_1

# set tuning for benchamrk model (2)
tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

# More complicated model with more features including amenities
set.seed(2021)
system.time({
  rf_model_2 <- train(
    formula(paste0("price ~", paste0(predictors_3, collapse = " + "))),
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity"
  )
})

rf_model_2


# evaluate random forests -------------------------------------------------

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)


# Turning parameter choice 1
result_1 <- matrix(c(
  rf_model_1$finalModel$mtry,
  rf_model_2$finalModel$mtry,
  rf_model_1$finalModel$min.node.size,
  rf_model_2$finalModel$min.node.size
),
nrow=2, ncol=2,
dimnames = list(c("Model 1", "Model 2"),
                c("Min vars","Min nodes"))
)
result_1

# Turning parameter choice 2
result_2 <- matrix(c(mean(results$values$`model_1~RMSE`),
                     mean(results$values$`model_2~RMSE`)
),
nrow=2, ncol=1,
dimnames = list(c("Model 1", "Model 2"),
                c(results$metrics[2]))
)
result_2


# Compare models ----------------------------------------------------------


final_models <-
  list("OLS" = ols_model,
       "OLS (basic + reviews)" = ols_model2,
       "LASSO (model with interactions)" = lasso_model,
       "CART" = cart_model,
       "Random forest basic + reviews)" = rf_model_1,
       "Random forest (with amenities)" = rf_model_2)

results <- resamples(final_models) %>% summary()
results

# Model selection is carried out on this CV RMSE
# CART and Random forest (smaller model) are performing worse
result_4 <- imap(final_models, ~{
  mean(results$values[[paste0(.y,"~RMSE")]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("CV RMSE" = ".")
result_4

# Based on MAE, RMSE, R-squared the second RF model is our final model with 10 min vars 5 min nodes


### evaluate preferred model on the holdout set

result_5 <- map(final_models, ~{
  RMSE(predict(.x, newdata = data_holdout), data_holdout[["price"]])
}) %>% unlist() %>% as.data.frame() %>%
  rename("Holdout RMSE" = ".")
result_5



# MODEL DIAGNOSTICS -------------------------------------------------------


# Variable Importance Plots -----------------------------------------------

# first need a function to calculate grouped varimp
group.importance <- function(rf.obj, groups) {
  var.imp <- as.matrix(sapply(groups, function(g) {
    sum(importance(rf.obj)[g], na.rm = TRUE)
  }))
  colnames(var.imp) <- "MeanDecreaseGini"
  return(var.imp)
}


rf_model_2_var_imp <- importance(rf_model_2$finalModel)/1000
rf_model_2_var_imp_df <-
  data.frame(varname = names(rf_model_2_var_imp),imp = rf_model_2_var_imp) %>%
  mutate(varname = gsub("f_neighbourhood_cleansed", "Borough:", varname) ) %>%
  mutate(varname = gsub("f_room_type", "Room type:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))


# full varimp plot, above a cutoff


# to have a quick look -> it does not really readable
plot(varImp(rf_model_2))

# make it more readable by showing only important ones
cutoff = 600
rf_model_2_var_imp_plot <- ggplot(rf_model_2_var_imp_df[rf_model_2_var_imp_df$imp>cutoff,],
                                  aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color="indianred3", size=1.5) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="indianred3", size=1) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6),
        axis.title.x = element_text(size=6), axis.title.y = element_text(size=6))
rf_model_2_var_imp_plot

save_fig("RandomForrest-varimp-base",data_out, "large")
# So these are the important features that drives our predictive model


# full varimp plot, top 10 only


# have a version with top 10 variables
rf_model_2_var_imp_plot_b <- ggplot(rf_model_2_var_imp_df[1:10,], aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color="indianred3", size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="indianred3", size=0.75) +
  ylab("Importance (Percent)") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_plot_b

save_fig("RandomForrest-varimp-top10",data_out, "small")


# arimp plot grouped

# grouped variable importance - keep binaries created off factors together
# Adding all the neighbourhoods

varnames <- rf_model_2$finalModel$xNames
f_neighbourhood_cleansed_varnames <- grep("f_neighbourhood_cleansed",varnames, value = TRUE)
f_cancellation_policy_varnames <- grep("f_cancellation_policy",varnames, value = TRUE)
f_bed_type_varnames <- grep("f_bed_type",varnames, value = TRUE)
f_property_type_varnames <- grep("f_property_type",varnames, value = TRUE)
f_room_type_varnames <- grep("f_room_type",varnames, value = TRUE)

groups <- list(f_neighbourhood_cleansed=f_neighbourhood_cleansed_varnames,
               f_cancellation_policy = f_cancellation_policy_varnames,
               f_bed_type = f_bed_type_varnames,
               f_property_type = f_property_type_varnames,
               f_room_type = f_room_type_varnames,
               f_bathroom = "f_bathroom",
               n_days_since = "n_days_since",
               n_accommodates = "n_accommodates",
               n_beds = "n_beds")

rf_model_2_var_imp_grouped <- group.importance(rf_model_2$finalModel, groups)
rf_model_2_var_imp_grouped_df <- data.frame(varname = rownames(rf_model_2_var_imp_grouped),
                                            imp = rf_model_2_var_imp_grouped[,1])  %>%
  mutate(imp_percentage = imp/sum(imp))

rf_model_2_var_imp_grouped_plot <-
  ggplot(rf_model_2_var_imp_grouped_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color="indianred3", size=1) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color="indianred3", size=0.7) +
  ylab("Importance (Percent)") +   xlab("Variable Name") +
  coord_flip() +
  # expand=c(0,0),
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(size=4), axis.text.y = element_text(size=4),
        axis.title.x = element_text(size=4), axis.title.y = element_text(size=4))
rf_model_2_var_imp_grouped_plot

save_fig("RandomForrest-varimp-group",data_out, "small")


# Partial Dependence Plots -------------------------------------------------------


pdp_n_acc <- pdp::partial(rf_model_2, pred.var = "n_accommodates", pred.grid = distinct_(data_holdout, "n_accommodates"), train = data_train)
pdp_n_acc %>%
  autoplot( ) +
  geom_point(color="indianred3", size=2) +
  geom_line(color="indianred3", size=1) +
  ylab("Predicted price") +
  xlab("Accommodates (persons)") +
  #scale_x_continuous(limit=c(1,7), breaks=seq(1,7,1))+
  theme_bw()


pdp_n_roomtype <- pdp::partial(rf_model_2, pred.var = "f_room_type", pred.grid = distinct_(data_holdout, "f_room_type"), train = data_train)
pdp_n_roomtype %>%
  autoplot( ) +
  geom_point(color="indianred3", size=2) +
  ylab("Predicted price") +
  xlab("Room type") +
  scale_y_continuous(limits=c(60,120), breaks=seq(60,120, by=10)) +
  theme_bw()


# Subsample performance: RMSE / mean(y) ---------------------------------------
# NOTE  we do this on the holdout set.

# ---- cheaper or more expensive flats - not used in book
data_holdout_w_prediction <- data_holdout %>%
  mutate(predicted_price = predict(rf_model_2, newdata = data_holdout))



######### create nice summary table of heterogeneity
a <- data_holdout_w_prediction %>%
  mutate(is_low_size = ifelse(n_accommodates <= 3, "small apt", "large apt")) %>%
  group_by(is_low_size) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

b <- data_holdout_w_prediction %>%
  filter(f_neighbourhood_cleansed %in% c("I Centro Storico", "II Parioli/Nomentano", "III Monte Sacro", "IV Tiburtina", "V Prenestino/Centocelle", "VI Roma delle Torri", "VII San Giovanni/Cinecitta", "VIII Appia Antica", "IX Eur", "X Ostia/Acilia", "XI Arvalia/Portuense", "XII Monte Verde", "XIII Aurelia", "XIV Monte Mario", "XV Cassia/Flaminia")) %>%
  group_by(f_neighbourhood_cleansed) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )

c <- data_holdout_w_prediction %>%
  filter(f_property_type %in% c("Apartment", "Condominium")) %>%
  group_by(f_property_type) %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = rmse / mean_price
  )


d <- data_holdout_w_prediction %>%
  dplyr::summarise(
    rmse = RMSE(predicted_price, price),
    mean_price = mean(price),
    rmse_norm = RMSE(predicted_price, price) / mean(price)
  )

# Save output
colnames(a) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(b) <- c("", "RMSE", "Mean price", "RMSE/price")
colnames(c) <- c("", "RMSE", "Mean price", "RMSE/price")
d<- cbind("All", d)
colnames(d) <- c("", "RMSE", "Mean price", "RMSE/price")

line1 <- c("Type", "", "", "")
line2 <- c("Apartment size", "", "", "")
line3 <- c("Borough", "", "", "")

result_3 <- rbind(line2, a, line1, c, line3, b, d) %>%
  transform(RMSE = as.numeric(RMSE), `Mean price` = as.numeric(`Mean price`),
            `RMSE/price` = as.numeric(`RMSE/price`))
result_3


# Actual versus predicted -------------------------------------------------


level_vs_pred <- ggplot(data = data_holdout_w_prediction) +
  geom_point(aes(y=predicted_price, x=price), color = "indianred3", size = 1,
             shape = 16, show.legend=FALSE, na.rm=TRUE) +
  geom_segment(aes(x = 0, y = 0, xend = 500, yend =500), size=0.5, color="black", linetype=2) +
  coord_cartesian(xlim = c(0, 500), ylim = c(0, 500)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 500), breaks=seq(0, 500, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 500), breaks=seq(0, 500, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bw() 
level_vs_pred


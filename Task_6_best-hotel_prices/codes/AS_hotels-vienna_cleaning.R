################################
##        Serfőző Attila      ##
##        Data Analysis 3     ##
##         Assignment 3       ##   
##          Preparation       ##
################################


# Set up environment ------------------------------------------------------

rm(list=ls())

dir <- "D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/Assignments/Assignment_3/"

data_in <- paste0(dir,"data/raw/")
data_out <- paste0(dir,"data/clean/")

# Import packages
library(dplyr)
library(tidyverse)
library(Hmisc)

# Import data
df <- read.csv(paste0(data_in,"hotelbookingdata-vienna.csv"), stringsAsFactors = F)


# Cleaning ----------------------------------------------------------------

# convert distance to center to numeric
df$n_distance <- as.numeric(gsub("[^0-9\\.]","",df$center1distance))
df$n_distance_alter <- as.numeric(gsub("[^0-9\\.]","",df$center2distance))

describe(df$n_distance)

# parsing accommodationtype column
# replace missing values to handle split
df$f_accommodation_type <- unlist(sapply(strsplit(as.character(df$accommodationtype), "@"), '[[', 2))
df <- df %>% mutate(f_accommodation_type = factor(f_accommodation_type))

describe(df$f_accommodation_type)

# generate numerical variable from rating variable
df$rating <- as.numeric(gsub("/5","",df$guestreviewsrating))

describe(df$rating)

# Rename variables
colnames(df)[colnames(df)=="rating"] <- "n_rating"
colnames(df)[colnames(df)=="rating_reviewcount"] <- "n_rating_count"
colnames(df)[colnames(df)=="rating2_ta"] <- "f_ratingta"
colnames(df)[colnames(df)=="rating2_ta_reviewcount"] <- "n_ratingta_count"
colnames(df)[colnames(df)=="addresscountryname"] <- "country"
colnames(df)[colnames(df)=="s_city"] <- "city"
colnames(df)[colnames(df)=="starrating"] <- "f_stars"

# Create factors
df <- df %>% mutate(f_stars = factor(f_stars))

describe(df$f_stars)

# drop if hotel id is missing
describe(df$hotel_id)
df <- df[!is.na(df$hotel_id), ]


# Drop duplicates
df[duplicated(df)==T,]
# drop if the row is the same based on the most important variables
df <- df[!duplicated(subset(df, select = c(city, hotel_id, n_distance, f_stars, n_rating, price, year, month, weekend, holiday))), ]

# Check dummies
df <- df %>% mutate(d_offer = as.numeric(offer))
df <- df %>% mutate(d_scarce_room = as.numeric(scarce_room))

# Create factors
df <- df %>% mutate(f_city_actual = factor(city_actual))
df <- df %>% mutate(f_neighbourhood = factor(neighbourhood))
df <- df %>% mutate(f_offer_cat = factor(offer_cat))

# drop vars
df <- df %>% select(-c(guestreviewsrating, center1distance, center2distance, accommodationtype, city_actual, offer, offer_cat, scarce_room, neighbourhood))
# drop vars because of lack of variety
df <- df %>% select(-c(country, city, center1label, center2label, price_night, weekend, holiday, year, month))

to_filter <- sapply(df, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# Replace missing variables re reviews with zero, when no review + add flags
df <- df %>%
  mutate(
    flag_rating=ifelse(is.na(n_rating),1, 0),
    n_rating =  ifelse(is.na(n_rating), median(n_rating, na.rm = T), n_rating),
    flag_rating_count=ifelse(is.na(n_rating_count),1, 0),
    n_rating_count =  ifelse(is.na(n_rating_count),1 , n_rating_count),
    flag_ratingta=ifelse(is.na(f_ratingta),1, 0),
    f_ratingta =  ifelse(is.na(f_ratingta), median(f_ratingta, na.rm = T), f_ratingta),
    flag_ratingta_count=ifelse(is.na(n_ratingta_count),1, 0),
    n_ratingta_count =  ifelse(is.na(n_ratingta_count),1 , n_ratingta_count),
    n_distance = ifelse(n_distance == 0, 0.1, n_distance)
  )

df <- df %>% mutate(f_ratingta = factor(f_ratingta))

# Create numerical variables
df <- df %>%
  mutate(
    price = as.numeric(price),
    hotel_id = as.numeric(hotel_id))

write.csv(df, paste0(data_out,"hotels-vienna.csv"), row.names = F)
write_rds(df,paste0(data_out,"hotels-vienna.rds"))

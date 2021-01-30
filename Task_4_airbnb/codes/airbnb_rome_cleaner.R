################################
##        Serfőző Attila      ##
##        Data Analysis 3     ##
##         Assignment 1       ##   
##          Clean data        ##
################################


# Set up environment ------------------------------------------------------

# Set libraries
rm(list=ls())
library(tidyverse)
library(Hmisc) # describe
library(ggplot2)
library(ggthemes)


# Set directory
dir<-"D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/DA3_R/Assignment 1"
setwd(dir)

data_in  <- paste0(dir,"/data/raw/")
data_out <- paste0(dir,"/data/clean/")


# Drop unnecessary variables to reduce file size
data <- read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location",
           "minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights","maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", "number_of_reviews_ltm", "is_business_travel_ready", "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms")
data<-data[ , !(names(data) %in% drops)]

write.csv(data,file=paste0(data_in,"airbnb_rome_listing.csv"))

rm(data,drops)


# Data Cleaning -----------------------------------------------------------


# Import data
df<-read.csv(paste0(data_in,"airbnb_rome_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)
              

# Format columns

# remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

# remove dollar signs from price variables
for (pricevars in c("price", "weekly_price","monthly_price","security_deposit","cleaning_fee","extra_people")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

# format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","is_location_exact","requires_license","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}


#--------------------------------------------------------------------------
### Amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

backup <- df
df <- backup

# Aggreagate amenities

column_names <- c("Pool", "Tub", "Wide entrance", "Air conditioning","Shower", "TV", "Essentials", "Oven", "Washer", "Kitchen",
                  "Internet|Ethernet|Wifi", "balcony|terrace", "wheelchair|flat path", "heating|heated floor")

for( i in column_names){

  # Search for columns which match the words
  new_df <- df %>% select(matches(i))
  
  # Check rows where the sum of the 0/1-s is larger than add 1 into the new column
  df$new_col <- ifelse(rowSums(new_df)>0, 1, 0)
  
  # Rename the new column
  names(df)[names(df) == 'new_col'] <- i
 
  # Remove the original columns
  df <- df %>% select(-colnames(new_df))
  
}

names(df)[names(df) == 'Internet|Ethernet|Wifi'] <- 'Internet'
names(df)[names(df) == 'balcony|terrace'] <- 'Balcony'
names(df)[names(df) == 'wheelchair|flat path'] <- 'Wheelchair accessible'
names(df)[names(df) == 'heating|heated floor'] <- 'Heating'

rm(new_df, column_names, i)

### Drop unnecessary variables

drops <- c(' toilet','Accessible-height bed','Accessible-height toilet','Air purifier','Baby bath','Baby monitor',
           'Babysitter recommendations','Bath towel','Beach view','Beachfront','Bed linens','Bedroom comforts',
           'Bidet','Body soap','Breakfast table','Building staff','Ceiling fan','Ceiling hoist','Changing table',
           'Childrenâ€™s books and toys','Childrenâ€™s dinnerware','Cleaning before checkout','Coffee maker',
           'Cooking basics','Crib','Day bed','Dishes and silverware','DVD player','Electric profiling bed',
           'En suite bathroom','Espresso machine','Exercise equipment','Extra pillows and blankets',
           'Extra space around bed','Fax machine','Fire pit','Fireplace guards','Firm mattress',
           'Fixed grab bars for toilet','Formal dining area','Game console','Garden or backyard','Gazebo',
           'Ground floor access','Hammam','Hand or paper towel','Hand soap','HBO GO','Heat lamps','Heated towel rack',
           'High-resolution computer monitor','High chair','Host greets you','Hot water','Hot water kettle',
           'Lake access','Long term stays allowed','Luggage dropoff allowed','Memory foam mattress','Microwave',
           'Mini fridge','Mobile hoist','Mountain view','Mudroom','Murphy bed','Nespresso machine','Netflix',
           'No stairs or steps to enter','Other','Outdoor parking','Outdoor seating','Outlet covers',
           'Pack â€™n Play/travel crib','Parking','Piano','Pillow-top mattress','Printer','Private bathroom',
           'Projector and screen','Refrigerator','Restaurant','Room-darkening shades','Safe','Safety card',
           'Sauna','Shampoo','Single level home','Ski-in/Ski-out','Sound system','Stair gates','Standing valet',
           'Stove','Sun loungers','Table corner guards','Toilet paper','Touchless faucets','Warming drawer',
           'Waterfront','Well-lit path to entrance','Wet bar','Wide doorway to guest bathroom','Wide entryway',
           'Wide hallways','Window guards','Wine cooler')
df<-df[ , !(names(df) %in% drops)]

# Drop kept amenities with small amount of true or false observations
tdf <- df %>% select(73:126)

less_than_3 <- tdf %>% select(where(~mean(. == 1) <= 0.003))
more_than_3 <- tdf %>% select(where(~mean(. == 1) >= 0.97))

df <- df %>% select(-colnames(less_than_3))
df <- df %>% select(-colnames(more_than_3))

#write csv
write.csv(df,file=paste0(data_out,"airbnb_rome_cleaned.csv"))

rm(backup, less_than_3, more_than_3, tdf, binary, drops, levs, perc, pricevars)

# Prepare Data ------------------------------------------------------------

data <- df

# Research question applies to appartments for 2-6 guests

###
# Keep only properties with number of guests between 2-6
data <- data %>%  filter(data$accommodates >= 2 & data$accommodates <= 6)

###
# Keep if property type Apartment, Condominium or 
table(data$property_type)
data <- data %>% filter(property_type %in% c("Apartment", "Condominium", "Serviced apartment" ))

# Rename Serviced apartment to Apartment and property type create factor
data <- data %>%
  mutate(
    property_type = ifelse(data$property_type == "Serviced apartment", "Apartment", data$property_type),
    f_property_type = factor(property_type))

###
# Rename room type
table(data$room_type)

data$room_type <- data$room_type %>% replace(data$room_type == 'Entire home/apt', "Entire_Apt")
data$room_type <- data$room_type %>% replace(data$room_type == 'Hotel room', "Private")
data$room_type <- data$room_type %>% replace(data$room_type == 'Private room', "Private")
data$room_type <- data$room_type %>% replace(data$room_type == 'Shared room', "Shared")

# Create room type factor
data <- data %>% mutate(f_room_type = factor(room_type))

###
# Cancellation policy as factor
table(data$cancellation_policy)

# Convert super strict 30, 60, 95 to strict
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'strict_new', "strict")
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'super_strict_30_new', "strict")
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'super_strict_60_new', "strict")
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'luxury_super_strict_95', "strict")
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'moderate_new', "moderate")
data$cancellation_policy <- data$cancellation_policy %>% replace(data$cancellation_policy == 'flexible_new', "flexible")

# Create factor
data <- data %>% mutate(f_cancellation_policy = factor(cancellation_policy))

###
# Bed_type and Neighbourhood_cleansed as factors
table(data$bed_type)
table(data$neighbourhood)

# Rename to Couch
data <- data %>% mutate(bed_type = ifelse(bed_type %in% c("Futon", "Pull-out Sofa", "Airbed"), "Couch", bed_type),
                        f_bed_type = factor(bed_type),
                        f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

#--------------------------------------------------------------------------

## Create Numerical variables
data <- data %>% mutate( usd_price_day = price, p_host_response_rate = as.numeric(host_response_rate))
# rename cleaning_fee column
data <- data %>% rename(usd_cleaning_fee = cleaning_fee)

#--------------------------------------------------------------------------

# add new numeric columns from certain columns
numericals <- c("accommodates","bathrooms","review_scores_rating","number_of_reviews","guests_included",
                "reviews_per_month","extra_people","minimum_nights","beds")
data <- data %>% mutate_at(vars(numericals), funs("n"=as.numeric))


# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)


# create days since first review
data <- data %>% mutate(n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                                    as.Date(first_review ,format="%Y-%m-%d")))

# create dummy vars
dummies <- data %>% select(73:123)
dummies <- colnames(dummies)
data <- data %>% mutate_at(vars(dummies), funs("d"= (.)))

# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^usd_.*"), price, id,
         neighbourhood_cleansed,cancellation_policy,room_type,property_type)

# Drop observations lacking price
data <- data %>% drop_na(price)

write_csv(data, paste0(data_out, "airbnb_london_workfile.csv"))


# Data Exploration --------------------------------------------------------

df <- data
data <- df

#######
# Price
#######

summary(data$price)
describe(data$price)

# Remove extreme values
data <- data %>%
  filter(price <= 750)

# Histograms
ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Price") +
  theme_bw()

# Take log of price
data <- data %>% mutate(ln_price = log(price))

ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Log price") +
  theme_bw()

#############
# Accomodates
#############

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, shape=16)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour="indianred3", se=FALSE)+
  theme_bw()

# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)


######
# Beds
######

data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

ggplot(data = data, aes(x=n_beds, y=price)) +
  geom_point(size=1, shape=16)+
  labs(x="Number of beds",y="Price")+
  geom_smooth(method="lm", colour="indianred3", se=FALSE)+
  theme_bw()

# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))


############
## bathrooms
############

ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Nr of bathrooms") +
  theme_bw()

# Pool accomodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())


#####################
## Number of reviews
#####################

nreview_plot <- data %>%
  filter(n_number_of_reviews <500)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 10, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Nr of reviews") +
  theme_bw()

# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = "indianred3", color = "black") +
  ylab("Count") +
  xlab("Log Nr of reviews") +
  theme_bw()

# Group Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))

data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())


#############
## Time since
#############

# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

skimr::skim(data$n_number_of_reviews)
ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, color="indianred3", shape=4) +
  ylim(60,100)+
  xlim(0,50)+
  geom_smooth(method="loess", colour="black", se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bw()


#######################
## review score effect
#######################

ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, color="indianred3", shape=4) +
  geom_smooth(method="loess", colour="black", se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bw()

# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is


#################
## minimum nights
#################

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

#--------------------------------------------------------------------------

# look at categoricals

categoricals <- c("f_property_type", "f_room_type", "f_cancellation_policy", "f_bed_type")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}

#--------------------------------------------------------------------------

# Change Infinite values with NaNs
for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data, paste0(data_out, "airbnb_hackney_workfile_adj.csv"))


# Treating missing values -------------------------------------------------


# where do we have missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  )

# where do we have missing values now
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )
table(data$flag_days_since)

# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]


# save workfile
write.csv(data, paste0(data_out, "airbnb_hackney_work.csv"), row.names = F)


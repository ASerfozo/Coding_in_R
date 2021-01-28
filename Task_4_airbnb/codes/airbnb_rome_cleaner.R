################################
##        Serfőző Attila      ##
##        Data Analysis 3     ##
##         Assignment 1       ##   
##          Clean data        ##
################################

#setting working directory
rm(list=ls())

# CHANGE TO YOUR WORKING DIRECTORY
setwd("D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/DA3_R/Assignment 1")

dir<-"D:/Egyetem/CEU/Winter_Term/Data_Analysis_3/DA3_R/Assignment 1"

data_in  <- paste0(dir,"/data/raw/")
data_out <- paste0(dir,"/data/clean/")

library(tidyverse)

data<-read.csv(paste0(data_in,"listings.csv"))
drops <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location",
           "minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights","maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", "number_of_reviews_ltm", "is_business_travel_ready", "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms")
data<-data[ , !(names(data) %in% drops)]
drops <- c("minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights","maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", "number_of_reviews_ltm", "is_business_travel_ready", "calculated_host_listings_count_entire_homes", "calculated_host_listings_count_private_rooms", "calculated_host_listings_count_shared_rooms")

write.csv(data,file=paste0(data_in,"airbnb_rome_listing.csv"))

rm(data,drops)

#####################################

# opening dataset
df<-read.csv(paste0(data_in,"airbnb_rome_listing.csv"),
             sep=",",header = TRUE, stringsAsFactors = FALSE)
              
#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

#####################
#formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
for (pricevars in c("price", "weekly_price","monthly_price","security_deposit","cleaning_fee","extra_people")){
  df[[pricevars]]<-gsub("\\$","",as.character(df[[pricevars]]))
  df[[pricevars]]<-as.numeric(as.character(df[[pricevars]]))
}

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","is_location_exact","requires_license","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\{","",df$amenities)
df$amenities<-gsub("\\}","",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))


### Updated aggregate_columns function code ###
# Combine all sound system columns into 1 column.There are several different kinds of sound systems present.We would like to
# create one generic sound category.

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]


### Aggregate similar columns with for loop where possible

column_names <- c("Pool", "Wifi", "Tub", "Wide entrance", "Air conditioning","Shower", "TV", "Essentials", "Oven", "Washer", "Kitchen")

for( word in column_names){
  
  # Subset columns which contains a specific word and save them to another dataframe. Also select 'id' to use for merge later
  new_df <- df %>% select(contains(word),"id")
  
  #Go row by row to see if any of the rows have at least one '1'. If it does, populate new column 'col_name' with 1
  new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
  
  # Save new column and id column to another dataframe. We use this new dataframe to merge with original dataframe
  new_df_merge <- new_df %>% select(id,col_name)
  
  #merge original dataframe and new_df_merge by 'id'
  df <- merge(df,new_df_merge,by = "id", all = FALSE)
  
  #remove the new column and 'id' column from the new_df dataframe
  new_df <- new_df %>% select(-c(id,col_name))
  
  # Remove the subset columns from original dataframe since they have already been aggregated into a new column and merged
  df <- df %>% select(-colnames(new_df))
  
  # Rename the new column
  names(df)[names(df) == 'col_name'] <- word
  
}

# Aggregate columns with same meanings manually

word <- c("internet", "ethernet")
new_df <- df %>% select(contains(word),"id")
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
new_df_merge <- new_df %>% select(id,col_name)
df <- merge(df,new_df_merge,by = "id", all = FALSE)
new_df <- new_df %>% select(-c(id,col_name))
df <- df %>% select(-colnames(new_df))
names(df)[names(df) == 'col_name'] <- paste0("Internet","_agg")

word <- c("balcony", "terrace")
new_df <- df %>% select(contains(word),"id")
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
new_df_merge <- new_df %>% select(id,col_name)
df <- merge(df,new_df_merge,by = "id", all = FALSE)
new_df <- new_df %>% select(-c(id,col_name))
df <- df %>% select(-colnames(new_df))
names(df)[names(df) == 'col_name'] <- "Balcony"

word <- c("wheelchair", "flat path")
new_df <- df %>% select(contains(word),"id")
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
new_df_merge <- new_df %>% select(id,col_name)
df <- merge(df,new_df_merge,by = "id", all = FALSE)
new_df <- new_df %>% select(-c(id,col_name))
df <- df %>% select(-colnames(new_df))
names(df)[names(df) == 'col_name'] <- "Wheelchait accessible"

word <- c("heating", "heated floor")
new_df <- df %>% select(contains(word),"id")
new_df$col_name <- apply(new_df[0:ncol(new_df)], 1, function(x) ifelse(any(x == 1), '1', '0'))
new_df_merge <- new_df %>% select(id,col_name)
df <- merge(df,new_df_merge,by = "id", all = FALSE)
new_df <- new_df %>% select(-c(id,col_name))
df <- df %>% select(-colnames(new_df))
names(df)[names(df) == 'col_name'] <- "Heating"


### Drop unnecessary variables

drops <- c(' toilet','Accessible-height bed','Accessible-height toilet','Air purifier','Baby bath','Baby monitor','Babysitter recommendations','Bath towel','Beach view','Beachfront','Bed linens','Bedroom comforts','Bidet','Body soap','Breakfast table','Building staff','Ceiling fan','Ceiling hoist','Changing table','Childrenâ€™s books and toys','Childrenâ€™s dinnerware','Cleaning before checkout','Coffee maker','Cooking basics','Crib','Day bed','Dishes and silverware','DVD player','Electric profiling bed','En suite bathroom','Espresso machine','Exercise equipment','Extra pillows and blankets','Extra space around bed','Fax machine','Fire pit','Fireplace guards','Firm mattress','Fixed grab bars for toilet','Formal dining area','Game console','Garden or backyard','Gazebo','Ground floor access','Hammam','Hand or paper towel','Hand soap','HBO GO','Heat lamps','Heated towel rack','High-resolution computer monitor','High chair','Host greets you','Hot water','Hot water kettle','Lake access','Long term stays allowed','Luggage dropoff allowed','Memory foam mattress','Microwave','Mini fridge','Mobile hoist','Mountain view','Mudroom','Murphy bed','Nespresso machine','Netflix','No stairs or steps to enter','Other','Outdoor parking','Outdoor seating','Outlet covers','Pack â€™n Play/travel crib','Parking','Piano','Pillow-top mattress','Printer','Private bathroom','Projector and screen','Refrigerator','Restaurant','Room-darkening shades','Shampoo','Single level home','Ski-in/Ski-out','Sound system','Stair gates','Standing valet','Stove','Sun loungers','Table corner guards','Toilet paper','Touchless faucets','Warming drawer','Waterfront','Well-lit path to entrance','Wet bar','Wide doorway to guest bathroom','Wide entryway','Wide hallways','Window guards','Wine cooler')
df<-df[ , !(names(df) %in% drops)]


#write csv
write.csv(df,file=paste0(data_out,"airbnb_rome_cleaned.csv"))

############################
#       Attila Serfőző     #
#   Data Visualization 2   #
############################

library(data.table)
library(tidyverse)

# Get data ----------------------------------------------------------------

# Install tidy tuesday package
#install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2020-07-07')
tuesdata <- tidytuesdayR::tt_load(2020, week = 28)
tuesdata

coffee_ratings <- as.data.table(tuesdata$coffee_ratings)
str(coffee_ratings)

skimr::skim(coffee_ratings)



# Data Cleaning and transformation ----------------------------------------

# Keep main attribute variables with few number of missing values and cup point attributes
coffee_ratings <- coffee_ratings %>%  select(c("species", "owner", "country_of_origin", "region", "in_country_partner", "grading_date", "altitude_mean_meters", 
                                               "total_cup_points", "aroma", "flavor", "aftertaste", "acidity", "body", "balance", "uniformity", "clean_cup", "sweetness", "cupper_points"))

# Check summary statistics
summary(coffee_ratings)

# Remove observations without country name and observations without evaluation or extreme altitude
coffee_ratings <- filter(coffee_ratings, country_of_origin != "NA",
                         total_cup_points > 0,
                         altitude_mean_meters < 5000)

# Convert Grading date to date format and create grading year
library(lubridate)
coffee_ratings <- coffee_ratings %>%  mutate(grading_date = mdy(coffee_ratings$grading_date))
coffee_ratings <- coffee_ratings %>%  mutate(grading_year = year(coffee_ratings$grading_date))

# Add continents to data
continents <- read_csv(paste0("D:/Egyetem/CEU/Winter_Term/Data_Visualization_2/DV2_projects","/continents.csv"))
coffee_ratings <- merge(coffee_ratings, continents, all.x = TRUE, by.x = "country_of_origin", by.y="Country")

# Rename countries
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "Cote d?Ivoire"] <- "Ivory Coast"
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "Tanzania, United Republic Of"] <- "Tanzania"
coffee_ratings$country_of_origin[coffee_ratings$country_of_origin == "United States"
                                 | coffee_ratings$country_of_origin == "United States (Hawaii)"
                                 | coffee_ratings$country_of_origin == "United States (Puerto Rico)"] <- "USA"


# Exploratory Data Analysis -----------------------------------------------

# Summary statistics
summary(coffee_ratings)

# Quick check on all histograms
coffee_ratings %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram(color = "black", fill = "Sienna")+
  theme_bw()


evaluation <- coffee_ratings %>%  select(c("aroma","flavor","aftertaste","acidity","body","balance","uniformity","clean_cup","sweetness","cupper_points"))

library(GGally)
ggpairs(evaluation)

# Analysis ----------------------------------------------------------------

####
# Arabica vs Robusta

ggplot(coffee_ratings, aes(factor(species),total_cup_points)) + 
  geom_boxplot(color = "black", fill = c("Sienna","Peru")) +
  labs(title = "Distribution of coffee ranks by species", x = "Species", y = "Avg. Total Cup Points")


options(digits = 3)

coffee_ratings[,list(observations = .N, avg_tot_cup_points = mean(total_cup_points)), by = species]

####
# Which countries ranked the countries with best overall coffee quality?

# Top ranked countries in average
country_ranking <- coffee_ratings[,list(observations = .N, avg_tot_cup_points = mean(total_cup_points)), by = country_of_origin][order(-avg_tot_cup_points)]
country_ranking

# Considering only countries with more than 5 observed coffee type
country_ranking[observations>5,]

####
# South America or Africa has better coffee?

afr_vs_southam <- coffee_ratings[Continent == "Africa" | Continent == "South America",list(observations = .N, avg_tot_cup_points = mean(total_cup_points)), by = Continent][order(-avg_tot_cup_points)]
afr_vs_southam

ggplot(coffee_ratings, aes(factor(Continent),total_cup_points)) + 
  geom_boxplot(color = "black", fill = c("Sienna", "Peru", "Burlywood", "Wheat")) +
  labs(title = "Distribution of coffee ranks by continent", x = "Continent", y = "Avg. Total Cup Points")

####
# How did continents coffee quality performed during time?

point_year <- coffee_ratings[grading_year<2018, list( avg_tot_cup_points = round(mean(total_cup_points),2)), by = list(Continent, grading_year)]

ggplot(point_year, aes(grading_year,avg_tot_cup_points, group = Continent, color = Continent)) + 
  geom_line(size = 1) +
  scale_color_manual(values = c("Sienna", "Peru", "Burlywood", "Wheat")) +
  labs(title = "Average Total cup points of continents by year of grading", x = "Grading Year", y = "Avg. Total Cup Points")

# Animated chart
library(gganimate)

p <- ggplot(point_year, aes(x = Continent,y = avg_tot_cup_points)) + 
  geom_col(fill = c("Sienna")) +
  transition_states(grading_year) +
  geom_text(aes(label = round(avg_tot_cup_points,2)), vjust = -0.5)+
  coord_cartesian(ylim = c(75,90)) +
  labs(title = "Distribution of coffee ranks by continent {closest_state}", x = "Continent", y = "Avg. Total Cup Points")

animate(p, fps = 10, duration = 40, width = 800, height = 600, end_pause = 2)


####
# Which is the best altitude for coffee growing

round(coffee_ratings$altitude_mean_meters, -2)

point_altitude <- coffee_ratings[, list( avg_tot_cup_points = mean(total_cup_points)), by = round(coffee_ratings$altitude_mean_meters, -2)]


ggplot( point_altitude , aes(x = round , y = avg_tot_cup_points)) +
  geom_col(fill='Sienna') +
  coord_cartesian(ylim = c(70,90)) +
  geom_abline(slope=0, intercept=mean(coffee_ratings$total_cup_points),  col = "navyblue",lty=2) +
  labs(y="Avg. Total cup points", x = "Altitude in meters", title="Coffee quality versus above sea level altitude in meters")

mean(coffee_ratings$total_cup_points)

####
# Which is the best coffee region?

# Best coffees (regions)
region_ranking <- coffee_ratings[,list(observations = .N, avg_tot_cup_points = mean(total_cup_points)), by = list(country_of_origin, region)][order(-avg_tot_cup_points)]

# Top coffee (region)
head(unique(region_ranking),20)

# Top coffee (region) with at least 3 observed coffee
head(unique(region_ranking[observations>2,]),20)





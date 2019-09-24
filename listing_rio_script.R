library(tidyverse)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(leaflet)
library(kableExtra)
library(RColorBrewer)
library(plotly)

listings_rio <- read.csv("~/Desktop/listings_rio_detailed.csv", 
                   encoding="UTF-8", stringsAsFactors = F, na.strings = c(""))

# Clean dataset
airbnb <- {select(listings_rio, host_name, neighbourhood,latitude,longitude,room_type,price,minimum_nights,number_of_reviews,last_review,reviews_per_month,calculated_host_listings_count,availability_365)}

# Change price from currency ($12.1) to int (12)
airbnb$price <- as.numeric(gsub('\\$|,', '', airbnb$price))

# ---------------------------------
# head(filtered_listing) %>% kable() %>% kable_styling()

# Summary (Quartes, Median, Max/Min)
summary(airbnb)

# StringFactor
names_to_factor <- c("host_name", "neighbourhood", "room_type")
airbnb[names_to_factor] <- map(airbnb[names_to_factor], as.factor)

# GRAPH -> verify missing data

missing_airbnb <- airbnb %>% summarise_all(~(sum(is.na(.))/n()))
missing_airbnb <- gather(missing_airbnb, key = "variables", value = "percent_missing")
missing_airbnb <- missing_airbnb[missing_airbnb$percent_missing > 0.0, ] 
ggplot(missing_airbnb, aes(x = reorder(variables, percent_missing), y = percent_missing)) +
  geom_bar(stat = "identity", fill = "red", aes(color = I('white')), size = 0.3)+
  xlab('variables')+
  coord_flip() + 
  ggtitle("Missing Data") +
  xlab("Column name") +
  ylab("Percentage missing") 

# Remove missing data
airbnb <- na.omit(airbnb)


# -------------------------------------------------------------
# GRAPHS
ggplot(airbnb, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  ggtitle("Distribution of price",
          subtitle = "The distribution is very skewed") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3)

#-----------------------------------------------------------------
#
airbnb %>% filter(price >= mean(price)) %>% group_by(neighbourhood, room_type) %>% tally %>% 
  ggplot(aes(reorder(neighbourhood,desc(n)), n, fill = room_type)) +
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of above average price objects",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity")

#------------------------------------------------------------------

ggplot(airbnb, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(airbnb$price), color = "purple", linetype = 2)
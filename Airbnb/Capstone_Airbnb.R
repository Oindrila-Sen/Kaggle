library(tidyr)
library(dplyr)
library(ggplot2)
########################################
## 1. Loading Data
########################################
setwd(
  "/Users/oindrilasen/WORK_AREA/Data Science/Springboard/Exercise/Capstone Project/Data Sets"
)
Detail_listing <- read.csv(
  "Detail_listings.csv",
  header = TRUE,
  na.strings = "",
  stringsAsFactors = FALSE
)
glimpse(Detail_listing)
dim(Detail_listing)
########################################
## 2. Data Wrangling
########################################
# Eliminate useless columns:
# Remove some of the columns that won't be useful based on the goals
clean_listing <- select(
  Detail_listing,
  id,
  accommodates,
  host_response_rate,
  host_is_superhost,
  host_has_profile_pic,
  host_identity_verified,
  host_listings_count,
  instant_bookable,
  property_type,
  room_type,
  bathrooms,
  bedrooms,
  beds,
  bed_type,
  cancellation_policy,
  availability_365,
  price,
  weekly_price,
  monthly_price,
  cleaning_fee,
  security_deposit,
  number_of_reviews,
  review_scores_rating
)
glimpse(clean_listing)
# Remove "$" and "," from the price columns
# Transforming prices to number
clean_listing$price <-
  gsub("\\$", replacement = "", clean_listing$price)
clean_listing$price <-
  gsub(",", replacement = "", clean_listing$price)
clean_listing$price <- as.numeric(clean_listing$price)

clean_listing$security_deposit <-
  gsub("\\$", replacement = "", clean_listing$security_deposit)
clean_listing$security_deposit <-
  gsub(",", replacement = "", clean_listing$security_deposit)
clean_listing$security_deposit <-
  as.numeric(clean_listing$security_deposit)

clean_listing$cleaning_fee <-
  gsub("\\$", replacement = "", clean_listing$cleaning_fee)
clean_listing$cleaning_fee <-
  gsub(",", replacement = "", clean_listing$cleaning_fee)
clean_listing$cleaning_fee <- as.numeric(clean_listing$cleaning_fee)

clean_listing$weekly_price <-
  gsub("\\$", replacement = "", clean_listing$weekly_price)
clean_listing$weekly_price <-
  gsub(",", replacement = "", clean_listing$weekly_price)
clean_listing$weekly_price <- as.numeric(clean_listing$weekly_price)

clean_listing$monthly_price <-
  gsub("\\$", replacement = "", clean_listing$monthly_price)
clean_listing$monthly_price <-
  gsub(",", replacement = "", clean_listing$monthly_price)
clean_listing$monthly_price <-
  as.numeric(clean_listing$monthly_price)

# Transforming categorical Variables to factors:
to_factor <- c(
  'host_has_profile_pic',
  'host_response_rate',
  'host_is_superhost',
  'instant_bookable',
  'host_identity_verified',
  'property_type',
  'room_type',
  'bed_type',
  'cancellation_policy'
)
for (col in to_factor) {
  clean_listing[[col]] <- factor(clean_listing[[col]])
}
# before continuing let's deal with NA's
sapply(clean_listing, function (x)
  sum(is.na(x)))
########################################
## 3. Data Cleaning
########################################
## Replacing the NAs with the most common value for that column
table(clean_listing$bedrooms)
clean_listing$bedrooms[is.na(clean_listing$bedrooms)] <- 1

table(clean_listing$bathrooms) 
clean_listing$bathrooms[is.na(clean_listing$bathrooms)] <-1

table(clean_listing$beds) 
clean_listing$beds[is.na(clean_listing$beds)] <- 1
## Assuming that NAs in the price columns are 0
clean_listing$cleaning_fee[is.na(clean_listing$cleaning_fee)] <- 0
clean_listing$security_deposit[is.na(clean_listing$security_deposit)] <-
  0
clean_listing$weekly_price[is.na(clean_listing$weekly_price)] <- 0
clean_listing$monthly_price[is.na(clean_listing$monthly_price)] <- 0

## Replacing the NAs with the Median value of that column
clean_listing$review_scores_rating[is.na(clean_listing$review_scores_rating)] <-  median(clean_listing$review_scores_rating, na.rm = TRUE)
# convert the records with "N/A" values to "NA"
levels(clean_listing$host_response_rate)[levels(clean_listing$host_response_rate) ==
                                           "N/A"] <- NA
# convert the NA records to "0%"
clean_listing$host_response_rate[is.na(clean_listing$host_response_rate)] <- "0%"
# Remove the % sign
clean_listing$host_response_rate <-
  gsub("\\%", replacement = "", clean_listing$host_response_rate)
# Convert the variable to nemeric
clean_listing$host_response_rate <-as.numeric(clean_listing$host_response_rate)
## Assuming that NAs in the host_identity_verified columns are False
clean_listing$host_identity_verified[is.na(clean_listing$host_identity_verified)] <-
  "f"
## Assuming that NAs in the host_has_profile_pic columns are False
clean_listing$host_has_profile_pic[is.na(clean_listing$host_has_profile_pic)] <-
  "f"
## Replacing the NAs with 1 for host_listings_count
clean_listing$host_listings_count[is.na(clean_listing$host_listings_count)] <-
  1
## Assuming that NAs in the host_is_superhost columns are False
clean_listing$host_is_superhost[is.na(clean_listing$host_is_superhost)] <- "f"
# Check again for NA values
sapply(clean_listing, function (x)
  sum(is.na(x)))
# Modify the levels of  Property_type variable
levels(clean_listing$property_type)
levels(clean_listing$property_type)[2:17] <- "Others"
levels(clean_listing$property_type)[4:20] <- "Others"

# take a subset of the recors with price <= 500
clean_listing <-
  clean_listing %>%
  filter(price <= 500)
dim(clean_listing)

# Change the few observations that have cancelation policy of super_strict_30 and super_strict_60 to strict
table(clean_listing$cancellation_policy)
levels(clean_listing$cancellation_policy)[levels(clean_listing$cancellation_policy) ==
                                            "super_strict_30"] <- "strict"
levels(clean_listing$cancellation_policy)[levels(clean_listing$cancellation_policy) ==
                                            "super_strict_60"] <- "strict"
########################################
## 3. Add New Features
########################################
# Add 2 new columns has_weekly_discount and has_monthly_discount
# Add 2 new columns has_weekly_discount and has_monthly_discount
clean_listing$has_weekly_discount <- 
  ifelse(((clean_listing$price * 7) - clean_listing$weekly_price) > 0 & clean_listing$weekly_price!= 0, "Yes", "No" )
clean_listing$has_monthly_discount <- ifelse(((clean_listing$price * 30) - clean_listing$monthly_price) > 0  & clean_listing$monthly_price!= 0, "Yes", "No" )

# Add a new column good_host_response
clean_listing$good_host_response <- ifelse(clean_listing$host_response_rate  > 60, "Yes", "No" )
########################################
## 3. Exploratory Data Analysis:
########################################
## 3.1. Understand the variables individually
# 1. Price
ggplot(clean_listing, aes(x = price)) +
  geom_histogram(color = 'black',
                 fill = 'light blue',
                 na.rm = TRUE) +
  scale_x_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) +
  scale_y_continuous(limits = c(0, 6000), breaks = seq(0, 6000, 500))+
  ggtitle("Price Trends for Airbnb Listings in LA") 
# 2. Property_type & Room_type
table(clean_listing$property_type, clean_listing$room_type)
ggplot(clean_listing, aes(x = property_type, fill = room_type)) +
  geom_bar(na.rm = TRUE) 
# 3. Bed Type
ggplot(clean_listing, aes(x = bed_type)) +
  geom_bar(fill = 'light green', na.rm = TRUE)
# 4. Cancellation_Policy
ggplot(clean_listing, aes(x = cancellation_policy)) +
  geom_bar(fill = 'blue', na.rm = TRUE)
# 5. host_is_superhost
prop.table(table(clean_listing$host_is_superhost))
ggplot(clean_listing, aes(x = host_is_superhost)) +
  geom_bar(fill = 'light yellow')
# 6. host_identity_verified
prop.table(table(clean_listing$host_identity_verified))
ggplot(clean_listing, aes(x = host_identity_verified)) +
  geom_bar(fill = 'light blue')
# 7. instant_bookable
prop.table(table(clean_listing$instant_bookable))
ggplot(clean_listing, aes(x = instant_bookable)) +
  geom_bar(fill = 'yellow')
## check how the individual feature is influencing the price of a listing
# 1.plot price vs number_of_reviews
ggplot(clean_listing, aes(x = number_of_reviews, y = price)) +
  geom_point(size=2, shape=23) +
  stat_smooth(method = "loess", formula = clean_listing$price ~ clean_listing$number_of_reviews, size = 1)
  
scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50)) +
  scale_x_continuous(limits = c(0, 600), breaks = seq(0, 600, 100))
# 2.plot price vs bedrooms
ggplot(clean_listing, aes(x = factor(bedrooms), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(50, 500)
# 3.plot price vs bathrooms
ggplot(clean_listing, aes(x = factor(bathrooms), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(50, 500)
# 4.plot price vs cancellation_policy
ggplot(clean_listing, aes(x = cancellation_policy, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(50, 500)
# 5.plot price vs property_type
ggplot(clean_listing, aes(x = property_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 500)
# 6.plot price vs room_type
ggplot(clean_listing, aes(x = room_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 500)
# 7.plot price vs bed_type
ggplot(clean_listing, aes(x = bed_type, y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(00, 500)
# 8. plot price vs no of beds
ggplot(clean_listing, aes(x = factor(beds), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(0, 500)
# 9. plot price vs accomodates
ggplot(clean_listing, aes(x = factor(accommodates), y = price)) +
  geom_boxplot(na.rm = TRUE) +
  ylim(0, 500)
# 10. plot price vs host_response_rate
ggplot(clean_listing, aes(x = good_host_response, y = price)) +
  geom_bar(fill = 'light blue',na.rm = TRUE, stat = "identity") +
  scale_y_continuous(limits = c(0, 500), breaks = seq(0, 500, 50))
########################################
## 5. Create a Model
########################################
set.seed(150)
#Sample Indexes
indexes = sample(1:nrow(clean_listing), size = 0.2 * nrow(clean_listing))
# Split dataset into training and test set
test_data = clean_listing[indexes, ]
train_data = clean_listing[-indexes, ]

dim(train_data)
dim(test_data)

# Create a Linear Regression Model
lm_model <-
  lm(
    price ~  host_identity_verified + instant_bookable + room_type +                 bedrooms + bathrooms +
      beds + availability_365 +
      accommodates + security_deposit + host_is_superhost                     +
      cleaning_fee + property_type
    + has_weekly_discount + has_monthly_discount
    + good_host_response + cancellation_policy,
    data = train_data
  )
summary(lm_model)
plot(lm_model)
# Analyze the Model
predict.lm_model <- predict(lm_model, test_data)
head(predict.lm_model)
head(test_data$price)
SSE <- sum((test_data$price - predict.lm_model) ^ 2)
sqrt(SSE)
SST <- sum((test_data$price - mean(test_data$price)) ^ 2)
R_Squared_Value <- 1 - SSE / SST
R_Squared_Value

coefficients(lm_model)
       

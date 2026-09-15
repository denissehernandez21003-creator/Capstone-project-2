library(readxl)
library(dplyr)

# Load Dataset 
RDC_Inventory_Core_Metrics_Zip_History <- read_excel("~/Desktop/Capstone Project/RDC_Inventory_Core_Metrics_Zip_History.xlsx")
View(RDC_Inventory_Core_Metrics_Zip_History)

# ZIP Codes 
airport_zipcodes <- c(
  
  # Atlanta
  30337, 30354, 30297, 30305, 30339, 30328, 30309,
  
  # Dallas/Fort Worth
  76051, 75063, 75019, 75201, 76102, 76010, 75024,
  
  # Denver
  80249, 80019, 80022, 80202, 80203, 80206, 80226,
  
  # Los Angeles
  90045, 90301, 90245, 90012, 90210, 90028, 90401,
  
  # Chicago
  60018, 60176, 60601, 60201, 60005, 60302,
  
  # Minneapolis
  55425, 55423, 55120, 55401, 55101, 55424, 55109,
  
  # New York
  11434, 11420, 11414, 11375, 11354, 11103, 11101,
  
  # Boston
  2128, 2150, 2151, 2108, 2116, 2130, 2129
)

# Filter the data
airport_data <- RDC_Inventory_Core_Metrics_Zip_History %>%
  filter(postal_code %in% airport_zipcodes)

# View data
View(airport_data)


# Add new column for either near or further from airport
airport_data <- airport_data %>%
  mutate(
    airport_distance = case_when(
      
      postal_code %in% c(
        30337, 30354, 30297,
        76051, 75063, 75019,
        80249, 80019, 80022,
        90045, 90301, 90245,
        60018, 60176,
        55425, 55423, 55120,
        11434, 11420, 11414,
        2128, 2150, 2151
      ) ~ "Near Airport",
      
      postal_code %in% c(
        30305, 30339, 30328, 30309,
        75201, 76102, 76010, 75024,
        80202, 80203, 80206, 80226,
        90012, 90210, 90028, 90401,
        60601, 60201, 60005, 60302,
        55401, 55101, 55424, 55109,
        11375, 11354, 11103, 11101,
        2108, 2116, 2130, 2129
      ) ~ "Further from Airport"
    )
  )


# name airports
airport_data <- airport_data %>%
  mutate(
    airport = case_when(
      
      postal_code %in% c(
        30337, 30354, 30297, 30305, 30339, 30328, 30309
      ) ~ "ATL",
      
      postal_code %in% c(
        76051, 75063, 75019, 75201, 76102, 76010, 75024
      ) ~ "DFW",
      
      postal_code %in% c(
        80249, 80019, 80022, 80202, 80203, 80206, 80226
      ) ~ "DEN",
      
      postal_code %in% c(
        90045, 90301, 90245, 90012, 90210, 90028, 90401
      ) ~ "LAX",
      
      postal_code %in% c(
        60018, 60176, 60601, 60201, 60005, 60302
      ) ~ "ORD",
      
      postal_code %in% c(
        55425, 55423, 55120, 55401, 55101, 55424, 55109
      ) ~ "MSP",
      
      postal_code %in% c(
        11434, 11420, 11414, 11375, 11354, 11103, 11101
      ) ~ "JFK",
      
      postal_code %in% c(
        2128, 2150, 2151, 2108, 2116, 2130, 2129
      ) ~ "BOS"
    )
  )


# View data 
View(airport_data)


# Check how many rows are in each group
table(airport_data$airport_distance)

table(airport_data$airport)


# Summary of housing prices
summary(airport_data$median_listing_price)


# Average price for near and further from airport
distance_summary <- airport_data %>%
  group_by(airport_distance) %>%
  summarise(
    average_price = mean(median_listing_price, na.rm = TRUE)
  )

distance_summary


# Average price for each airport
airport_summary <- airport_data %>%
  group_by(airport) %>%
  summarise(
    average_price = mean(median_listing_price, na.rm = TRUE)
  )

airport_summary


# Average price for near and further at each airport
airport_distance_summary <- airport_data %>%
  group_by(airport, airport_distance) %>%
  summarise(
    average_price = mean(median_listing_price, na.rm = TRUE)
  )

airport_distance_summary



# Boxplot for near vs further
boxplot(
  median_listing_price ~ airport_distance,
  data = airport_data,
  main = "Housing Prices Near vs Further from Airports",
  xlab = "Distance from Airport",
  ylab = "Median Listing Price"
)


# Boxplot for each airport
boxplot(
  median_listing_price ~ airport,
  data = airport_data,
  main = "Housing Prices by Airport",
  xlab = "Airport",
  ylab = "Median Listing Price"
)


# Histogram of housing prices
hist(
  airport_data$median_listing_price,
  main = "Distribution of Housing Prices",
  xlab = "Median Listing Price"
)


# Bar graph for average price by airport
barplot(
  airport_summary$average_price,
  names.arg = airport_summary$airport,
  main = "Average Housing Price by Airport",
  xlab = "Airport",
  ylab = "Average Median Listing Price"
)


# Average housing price further from each airport
far_airport_average <- airport_data %>%
  filter(airport_distance == "Further from Airport") %>%
  group_by(airport) %>%
  summarise(
    average_price = mean(median_listing_price, na.rm = TRUE)
  )

# Bar graph
barplot(
  far_airport_average$average_price,
  names.arg = far_airport_average$airport,
  col = "darkblue",
  main = "Average Housing Price Further from Airport",
  xlab = "Airport",
  ylab = "Average Median Listing Price",
  ylim = c(0, 3000000)
)


# Average housing price near each airport
near_airport_average <- airport_data %>%
  filter(airport_distance == "Near Airport") %>%
  group_by(airport) %>%
  summarise(
    average_price = mean(median_listing_price, na.rm = TRUE)
  )

# Bar graph
barplot(
  near_airport_average$average_price,
  names.arg = near_airport_average$airport,
  col = "darkblue",
  main = "Average Housing Price Near Airport",
  xlab = "Airport",
  ylab = "Average Median Listing Price",
  ylim = c(0, 3000000)
)

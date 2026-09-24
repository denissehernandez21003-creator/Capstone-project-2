library(dplyr)
library(readxl)

Filtered_RDC_Inverntory <- read_excel("Filtered_RDC_Inverntory.xlsx")
View(Filtered_RDC_Inverntory)


# Add new column for either near or further from airport
airport_data <- airport_data %>%
  mutate(
    airport_distance = case_when(
      
      postal_code %in% c(
        30349, 30288, 30296,
        76051, 75063, 75019,
        80019, 80022, 80249,
        90094, 90250, 90266,
        60018, 60131,60706,
        55425, 55120, 55108,
        11580, 11598, 11375,
        02116, 02129, 02119
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


#2 Boxplots with the near and further (outliers removed)

#Removing Outliers for Boxplot

housing_no_outliers <- airport_data %>%
  group_by(airport) %>%
  filter(
    median_listing_price >= quantile(median_listing_price, 0.25, na.rm = TRUE) -
      1.5 * IQR(median_listing_price, na.rm = TRUE),
    
    median_listing_price <= quantile(median_listing_price, 0.75, na.rm = TRUE) +
      1.5 * IQR(median_listing_price, na.rm = TRUE)
  ) %>%
  ungroup()



#Boxplot with removed outliers

library(ggplot2)

ggplot(housing_no_outliers,
       aes(x = airport, y = median_listing_price)) +
  geom_boxplot() +
  labs(
    title = "Housing Prices by Airport",
    x = "Airport",
    y = "Housing Price"
  )


library(scales)
#DEN Boxplot

  ggplot(
    filter(housing_no_outliers, airport == "DEN"),
    aes(x = airport, y = median_listing_price)
  ) +
    geom_boxplot(
      width = 0.45,
      fill = "lightblue",
      alpha = 0.7
    ) +
    scale_y_continuous(
      labels = dollar_format()
    ) +
    labs(
      title = "Housing Prices Denver International Airport",
      x = NULL,
      y = "Median Listing Price"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 11),
      axis.title.y = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
  
  
#Boxplot All Airports (Outliers Removed)
  ggplot(
    housing_no_outliers,
    aes(x = airport, y = median_listing_price)
  ) +
    geom_boxplot(
      fill = "lightblue",
      alpha = 0.7,
      width = 0.6
    ) +
    scale_y_continuous(
      labels = dollar_format()
    ) +
    labs(
      title = "Housing Prices by Airport",
      x = "Airport",
      y = "Median Listing Price"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
 
 #Boxplot of All airports(Outliers Removed)  
  ggplot(
    housing_no_outliers,
    aes(
      x = airport,
      y = median_listing_price,
      fill = airport
    )
  ) +
    geom_boxplot(
      alpha = 0.75,
      width = 0.65
    ) +
    scale_y_continuous(
      labels = dollar_format()
    ) +
    labs(
      title = "Housing Prices Near Major U.S. Airports",
      x = "Airport",
      y = "Median Listing Price"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      axis.title = element_text(face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank()
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
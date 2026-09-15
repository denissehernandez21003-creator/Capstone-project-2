library(readxl)
library(tidyverse)

Airline_Delay_Cause <- read_excel("~/Documents/Airline_Delay_Cause.xlsx")
View(Airline_Delay_Cause)


# Look at the first few rows
head(Airline_Delay_Cause)

# Look at the column names
names(Airline_Delay_Cause)

# Look at the structure
str(Airline_Delay_Cause)

# Summary
summary(Airline_Delay_Cause)


delay_totals <- Airline_Delay_Cause %>%
  summarise(
    Carrier = sum(carrier_delay, na.rm = TRUE),
    Weather = sum(weather_delay, na.rm = TRUE),
    NAS = sum(nas_delay, na.rm = TRUE),
    Security = sum(security_delay, na.rm = TRUE),
    Late_Aircraft = sum(late_aircraft_delay, na.rm = TRUE)
  )

delay_totals



delay_graph <- delay_totals %>%
  pivot_longer(
    cols = everything(),
    names_to = "Delay_Cause",
    values_to = "Total_Delay_Minutes"
  )

delay_graph


ggplot(delay_graph, aes(x = Delay_Cause, y = Total_Delay_Minutes)) +
  geom_col() +
  labs(
    title = "Total Airline Delay Minutes by Cause",
    x = "Delay Cause",
    y = "Total Delay Minutes"
  )


## Top 10 airports with the most total delay minutes
airport_delay <- Airline_Delay_Cause %>%
  group_by(airport) %>%
  summarise(
    Total_Delay = sum(arr_delay, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Delay)) %>%
  slice_head(n = 10)

ggplot(airport_delay, aes(x = reorder(airport, Total_Delay),
                          y = Total_Delay)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Airports with the Most Delay Minutes",
    x = "Airport",
    y = "Total Delay Minutes"
  )





## Average delay minutes by cause
average_delays <- Airline_Delay_Cause %>%
  summarise(
    Carrier = mean(carrier_delay, na.rm = TRUE),
    Weather = mean(weather_delay, na.rm = TRUE),
    NAS = mean(nas_delay, na.rm = TRUE),
    Security = mean(security_delay, na.rm = TRUE),
    Late_Aircraft = mean(late_aircraft_delay, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Delay_Cause",
    values_to = "Average_Delay"
  )

ggplot(average_delays, aes(x = Delay_Cause, y = Average_Delay)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Average Delay Minutes by Cause",
    x = "Delay Cause",
    y = "Average Delay Minutes"
  )




# Top 10 airports with the most delayed flights
airport_delayed_flights <- Airline_Delay_Cause %>%
  group_by(airport) %>%
  summarise(
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  arrange(desc(Delayed_Flights)) %>%
  slice_head(n = 10)

ggplot(airport_delayed_flights,
       aes(x = reorder(airport, Delayed_Flights),
           y = Delayed_Flights,)) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Top 10 Airports with the Most Delayed Flights",
    x = "Airport",
    y = "Number of Delayed Flights"
  )



# Delay minutes by month
monthly_delays <- Airline_Delay_Cause %>%
  group_by(month) %>%
  summarise(
    Average_Delay = mean(arr_delay, na.rm = TRUE)
  )

ggplot(monthly_delays, aes(x = month, y = Average_Delay)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Airline Delay by Month",
    x = "Month",
    y = "Average Delay Minutes"
  )


# Delayed flights by month
monthly_flights <- Airline_Delay_Cause %>%
  group_by(month) %>%
  summarise(
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  )

ggplot(monthly_flights, aes(x = month, y = Delayed_Flights)) +
  geom_col() +
  labs(
    title = "Delayed Flights by Month",
    x = "Month",
    y = "Number of Delayed Flights"
  )

monthly_flights <- Airline_Delay_Cause %>%
  group_by(month) %>%
  summarise(
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  )

ggplot(monthly_flights, aes(x = factor(month, levels = 1:12, labels = month.abb),
                            y = Delayed_Flights)) +
  geom_col(fill = "black") +
  labs(
    title = "Delayed Flights by Month",
    x = "Month",
    y = "Number of Delayed Flights"
  )

# Find the number of delayed flights for each airline
airline_delayed_flights <- Airline_Delay_Cause %>%
  group_by(carrier_name) %>%
  summarise(
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  arrange(desc(Delayed_Flights))

# Look at the results
airline_delayed_flights

# Make the graph
ggplot(airline_delayed_flights,
       aes(x = reorder(carrier_name, Delayed_Flights),
           y = Delayed_Flights)) +
  geom_col(fill = "black") +
  coord_flip() +
  labs(
    title = "Airlines with the Most Delayed Flights",
    x = "Airline",
    y = "Number of Delayed Flights"
  )


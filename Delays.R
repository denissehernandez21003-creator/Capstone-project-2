library(readxl)
library(tidyverse)

Airline_Delay_Cause <- read_excel("Airline_Delay_Cause_2019_2026.xlsx")
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

#######################################################
# Find the number of delayed flights for each airline #
######################################################

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


##############################################################################
# Correlation coefficient between airport size and number of delayed flights #
##############################################################################

# Airport size and number of delayed flights
airport_size_delay <- Airline_Delay_Cause %>%
  group_by(airport) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  )

# Correlation coefficient
cor(airport_size_delay$Total_Flights,
    airport_size_delay$Delayed_Flights,
    use = "complete.obs")

# Graphic
ggplot(airport_size_delay,
       aes(x = Total_Flights,
           y = Delayed_Flights)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Airport Size vs. Number of Delayed Flights",
    x = "Total Number of Flights",
    y = "Number of Delayed Flights"
  )


##################################################################
# Percentage of delayed flights vs. frequency of delayed flights #
##################################################################

# Percentage and frequency of delayed flights
airport_delay_percentage <- Airline_Delay_Cause %>%
  group_by(airport) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Percentage = (Delayed_Flights / Total_Flights) * 100
  )

airport_delay_percentage

# Graphic
ggplot(airport_delay_percentage,
       aes(x = Delayed_Flights,
           y = Delay_Percentage)) +
  geom_point() +
  labs(
    title = "Delayed Flight Frequency vs. Percentage",
    x = "Number of Delayed Flights",
    y = "Percentage of Flights Delayed"
  )



#######################################
# Airport size vs. average delay time #
#######################################

# Airport size and average delay
airport_size_minutes <- Airline_Delay_Cause %>%
  group_by(airport) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Average_Delay = mean(arr_delay, na.rm = TRUE)
  )
airport_size_minutes

# Correlation coefficient
cor(airport_size_minutes$Total_Flights,
    airport_size_minutes$Average_Delay,
    use = "complete.obs")

# Graphic
ggplot(airport_size_minutes,
       aes(x = Total_Flights,
           y = Average_Delay)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Airport Size vs. Average Delay",
    x = "Total Number of Flights",
    y = "Average Delay Minutes"
  )

########################################
# Which airline had more delay causes  #
########################################

# Delay causes for each airline
airline_delay_causes <- Airline_Delay_Cause %>%
  group_by(carrier_name) %>%
  summarise(
    Carrier = sum(carrier_delay, na.rm = TRUE),
    Weather = sum(weather_delay, na.rm = TRUE),
    NAS = sum(nas_delay, na.rm = TRUE),
    Security = sum(security_delay, na.rm = TRUE),
    Late_Aircraft = sum(late_aircraft_delay, na.rm = TRUE)
  )
airline_delay_causes


airline_delay_graph <- airline_delay_causes %>%
  pivot_longer(
    cols = c(Carrier, Weather, NAS, Security, Late_Aircraft),
    names_to = "Delay_Cause",
    values_to = "Delay_Minutes"
  )

airline_delay_graph

# Graphic
ggplot(airline_delay_graph,
       aes(x = carrier_name,
           y = Delay_Minutes,
           fill = Delay_Cause)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Delay Causes by Airline",
    x = "Airline",
    y = "Total Delay Minutes",
    fill = "Delay Cause"
  )



library(readxl)
library(tidyverse)


# ============================================================
# IMPORT DATA
# ============================================================

# 2019-2026 Data
Airline_Delay_Cause <- read_excel("Airline_Delay_Cause_2019-2026.xlsx")

# 2010-2018 Data
Airline_Delay_Old <- read_excel("Airline_Delay_Cause_2010-2018.xlsx")


# Look at the data
View(Airline_Delay_Cause)
View(Airline_Delay_Old)

head(Airline_Delay_Cause)
names(Airline_Delay_Cause)
str(Airline_Delay_Cause)
summary(Airline_Delay_Cause)


# ============================================================
# 2010-2019 VS 2022-2026
# ============================================================

# Add labels for each time period
Airline_Delay_Cause <- Airline_Delay_Cause %>%
  mutate(Time_Period = "2022-2026")

Airline_Delay_Old <- Airline_Delay_Old %>%
  mutate(Time_Period = "2010-2019")


# Combine the datasets
airline_comparison <- bind_rows(
  Airline_Delay_Cause,
  Airline_Delay_Old
)

View(airline_comparison)


# Total and percentage of delayed flights
period_comparison <- airline_comparison %>%
  group_by(Time_Period) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Percentage = (Delayed_Flights / Total_Flights) * 100
  )

period_comparison


# Graphic - Number of delayed flights
ggplot(period_comparison,
       aes(x = Time_Period,
           y = Delayed_Flights)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Number of Delayed Flights: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Number of Delayed Flights"
  )


# Graphic - Percentage of delayed flights
ggplot(period_comparison,
       aes(x = Time_Period,
           y = Delay_Percentage)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Percentage of Delayed Flights: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Percentage of Flights Delayed"
  )


# ============================================================
# DELAY PERCENTAGE BY YEAR
# ============================================================

yearly_comparison <- airline_comparison %>%
  group_by(year) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Percentage = (Delayed_Flights / Total_Flights) * 100
  )

yearly_comparison


ggplot(yearly_comparison,
       aes(x = year,
           y = Delay_Percentage)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Percentage of Delayed Flights by Year",
    x = "Year",
    y = "Percentage of Flights Delayed"
  )




# percentage of delayed flights
delay_boxplot <- airline_comparison %>%
  mutate(
    Delay_Percentage = (arr_del15 / arr_flights) * 100
  )


# ============================================================
# BOXPLOT OF PERCENTAGE OF DELAYED FLIGHTS
# ============================================================

ggplot(delay_boxplot,
       aes(x = Time_Period,
           y = Delay_Percentage)) +
  geom_boxplot() +
  labs(
    title = "Percentage of Delayed Flights: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Percentage of Flights Delayed"
  )


# ============================================================
# BOXPLOT OF DELAY MINUTES
# ============================================================

ggplot(airline_comparison,
       aes(x = Time_Period,
           y = arr_delay)) +
  geom_boxplot() +
  labs(
    title = "Delay Minutes: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Delay Minutes"
  )


# ============================================================
# BAR CHART OF AVERAGE DELAY MINUTES
# ============================================================

average_period_delay <- airline_comparison %>%
  group_by(Time_Period) %>%
  summarise(
    Average_Delay = mean(arr_delay, na.rm = TRUE)
  )

average_period_delay

ggplot(average_period_delay,
       aes(x = Time_Period,
           y = Average_Delay)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Average Delay Minutes: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Average Delay Minutes"
  )


# ============================================================
# BAR CHART OF DELAY CAUSES
# ============================================================

period_delay_causes <- airline_comparison %>%
  group_by(Time_Period) %>%
  summarise(
    Carrier = sum(carrier_delay, na.rm = TRUE),
    Weather = sum(weather_delay, na.rm = TRUE),
    NAS = sum(nas_delay, na.rm = TRUE),
    Security = sum(security_delay, na.rm = TRUE),
    Late_Aircraft = sum(late_aircraft_delay, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(Carrier, Weather, NAS, Security, Late_Aircraft),
    names_to = "Delay_Cause",
    values_to = "Delay_Minutes"
  )

period_delay_causes

ggplot(period_delay_causes,
       aes(x = Delay_Cause,
           y = Delay_Minutes,
           fill = Time_Period)) +
  geom_col(position = "dodge") +
  labs(
    title = "Delay Causes: 2010-2019 vs. 2022-2026",
    x = "Delay Cause",
    y = "Total Delay Minutes",
    fill = "Time Period"
  )


# ============================================================
# BAR CHART OF DELAY PERCENTAGE
# ============================================================

period_delay_percentage <- airline_comparison %>%
  group_by(Time_Period) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Percentage = (Delayed_Flights / Total_Flights) * 100
  )

period_delay_percentage

ggplot(period_delay_percentage,
       aes(x = Time_Period,
           y = Delay_Percentage)) +
  geom_col(fill = "black") +
  labs(
    title = "Percentage of Delayed Flights: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Percentage of Flights Delayed"
  )


# ============================================================
# NUMBER OF DELAYED FLIGHTS BY TIME PERIOD
# ============================================================

period_delayed_flights <- airline_comparison %>%
  group_by(Time_Period) %>%
  summarise(
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  )

period_delayed_flights

ggplot(period_delayed_flights,
       aes(x = Time_Period,
           y = Delayed_Flights)) +
  geom_col(fill = "darkblue") +
  labs(
    title = "Number of Delayed Flights: 2010-2019 vs. 2022-2026",
    x = "Time Period",
    y = "Number of Delayed Flights"
  )


# ============================================================
# DELAY PERCENTAGE BY YEAR
# ============================================================

yearly_comparison <- airline_comparison %>%
  group_by(year) %>%
  summarise(
    Total_Flights = sum(arr_flights, na.rm = TRUE),
    Delayed_Flights = sum(arr_del15, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Percentage = (Delayed_Flights / Total_Flights) * 100
  )

yearly_comparison

ggplot(yearly_comparison,
       aes(x = year,
           y = Delay_Percentage)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Percentage of Delayed Flights by Year",
    x = "Year",
    y = "Percentage of Flights Delayed"
  )






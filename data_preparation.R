### Import packages
library(tidyverse)
library(lmtest)
library(stargazer)
library(estimatr)
library(mfx)
library(readxl)
library(dplyr)



### Set working directory
getwd() 
setwd('C:/Users/NEW/OneDrive/Tanulás/¤ Corvinus PhD/Econometrics/Project/') # wd Gergő
# setwd() # wd Martina 
# setwd() # wd Marci



### Import input data
fuel_prices_main <- read_excel("fuel_prices_data.xlsx")
settlement_data <- read_excel("settlement_data.xlsx")



######
### Data transformation
######

# join "fuel_prices_main" and "settlement_data" by settlement name
fuel_prices_main <- fuel_prices_main %>%
  left_join(settlement_data, by = c("Settlement" = "Name"))


# calculate aggregated daily average fuel prices - will be used to fill NAs
daily_avg <- fuel_prices_main %>%
  group_by(Date) %>%
  summarise(avg_diesel_price = mean(Diesel, na.rm = TRUE),
            avg_gasoline_price = mean(Diesel, na.rm = TRUE))


# calculate pct changes in fuel prices
daily_avg <- daily_avg %>%
  arrange(Date) %>%  
  mutate(
    diesel_change_pct = (avg_diesel_price - lag(avg_diesel_price)) / lag(avg_diesel_price) * 100,
    gasoline_change_pct = (avg_gasoline_price - lag(avg_gasoline_price)) / lag(avg_gasoline_price) * 100
  )


### Fill missing data

fuel_prices_main <- fuel_prices_main %>%
  mutate(date = as.Date(Date)) %>%
  arrange(Address, Date)

# join aggregated daily averages to "fuel_prices_main"
fuel_prices_main <- fuel_prices_main %>%
  left_join(daily_avg %>% select(Date, diesel_change_pct, gasoline_change_pct), 
            by = "Date")

# forward fill - with average daily price change
fuel_prices_main <- fuel_prices_main %>%
  mutate(diesel_filled = ifelse(
                  is.na(Diesel),
                  lag(Diesel) * (1 + diesel_change_pct / 100),
                  Diesel),
         gasoline_filled = ifelse(
                  is.na(Gasoline),
                  lag(Gasoline) * (1 + gasoline_change_pct / 100),
                  Gasoline)
         )

# backward fill - with average daily prices change
fuel_prices_main <- fuel_prices_main %>%
  mutate(diesel_filled = ifelse(
                  is.na(diesel_filled),
                  lead(diesel_filled) / (1 + lead(diesel_change_pct)),
                  diesel_filled),
         gasoline_filled = ifelse(
                  is.na(gasoline_filled),
                  lead(gasoline_filled) / (1 + lead(gasoline_change_pct)),
                  gasoline_filled)
        )

sum(is.na(fuel_prices_main$gasoline_filled))
sum(is.na(fuel_prices_main$diesel_filled))

# remove rows with NAs
fuel_prices_main <- fuel_prices_main %>%
  filter(!is.na(diesel_filled) & !is.na(gasoline_filled))

# remove unnecessary columns
fuel_prices_main <- fuel_prices_main %>%
  select(-Diesel, -Gasoline, -date, -diesel_change_pct, -gasoline_change_pct)



### Aggregate average diesel/gasoline prices in station level

fuel_prices_weekly <- fuel_prices_main %>%
  group_by(Address) %>%
  summarise(
    Diesel = mean(diesel_filled),
    Gasoline = mean(gasoline_filled),
    across(everything(), ~ first(.x)),  # keeps first value of other columns
    .groups = "drop"
  )

# remove some columns
fuel_prices_weekly <- fuel_prices_weekly %>%
  select(-diesel_filled, -gasoline_filled, -Date)



### Calculate the population/fuel station metric
fuel_prices_weekly <- fuel_prices_weekly %>%
  group_by(Settlement) %>%
  mutate(
    pop_per_station = Population / n()
  ) %>%
  ungroup()





######
### Modeling
######




















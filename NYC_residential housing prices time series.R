library(tidyverse)
library(lubridate)
library(stringr)
library(forecast)

# Set the working directory to your NYC Database folder
setwd("C:/Users/maxma/Documents/AD 571/Week 1/NYC Database/NYC Database/")

# Load the dataset files
borough <- read_csv("BOROUGH.csv")
neighborhood <- read_csv("NEIGHBORHOOD.csv")
building_class <- read_csv("BUILDING_CLASS.csv")
nyc_transaction_data  <- read_csv("NYC_TRANSACTION_DATA.csv")


# Prepare the data for residential sales
residentails <- nyc_transaction_data %>% 
  left_join(building_class, by = c("BUILDING_CLASS_FINAL_ROLL" = "BUILDING_CODE_ID")) %>%
  filter(TYPE == "RESIDENTIAL") %>%
  filter(GROSS_SQUARE_FEET != 0, SALE_PRICE != 0) %>%
  filter(year(SALE_DATE) >= 2020) %>%
  filter(NEIGHBORHOOD_ID == 187) %>%
  mutate(SALE_YEAR = year(SALE_DATE),
         SALE_QUARTER = quarter(SALE_DATE),
         N_UNITS = RESIDENTIAL_UNITS + COMMERCIAL_UNITS)

# Run the multiple linear regression model
model <- lm(SALE_PRICE ~ YEAR_BUILT + BUILDING_CLASS_FINAL_ROLL + GROSS_SQUARE_FEET + RESIDENTIAL_UNITS, data = residentails)
summary(model)


# Convert sale prices to a time series object
sales_ts <- ts(residentails$SALE_PRICE, frequency = 4)

# Forecast model using Exponential Smoothing
model2 <- ets(y = sales_ts, model = "AAA")
pred <- forecast(model2, h = 24)


# Plot the forecast
options(scipen = 999) # Avoid scientific notation
autoplot(pred)

# Create a forecast table
forecast_table <- data.frame(
  Quarter = time(pred$mean),
  Forecasted_Sales = as.numeric(pred$mean),
  Lower_CI = pred$lower[, "95%"],
  Upper_CI = pred$upper[, "95%"]
)
print(forecast_table)



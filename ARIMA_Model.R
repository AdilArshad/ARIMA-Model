# forecasting data for Australian Stock Market.
#intall.packages("xts")
library(pdfetch) # for fetching data online from Yahoo_finance
library(urca)    # for performing different tests
library(tseries)  # for indexing in timme series data
library(forecast) # for forecasting purpose
library(openair)  # For analyzing Air quality type data
library(ggplot2)  # for plotting of graphs
library(xts)      # for dealing with xts objects.
# for clearing environment
#rm(list = ls())

# importing data from direct Yahoo websites using pdfetch in Real Time
raw_data = pdfetch_YAHOO("^AORD")

# storing values of adjclose colomn to variable
Adj = raw_data[,"^AORD.adjclose"]

# checking missing values
missing_values = sum(is.na(Adj))
missing_indexes = which(is.na(Adj))
Adj_new = na.omit(Adj)
missing_indexes_1 = which(is.na(Adj_new))

#log(Adj_new)
# Assuming Adj_new is your xts object for plotting graph of origial values
plot(Adj_new, main = "Time Series Plot of ^AORD.adjclose", ylab = "^AORD.adjclose", xlab = "Date")
###############################################################
acf(Adj_new)   # ACF test
pacf(Adj_new)  # PACF test
adf.test(Adj_new) # adf test
###############################################################

# Assuming Adj_new is your xts object and fit is the fitted ARIMA model
fit <- auto.arima(Adj_new, stepwise = FALSE, approximation = FALSE)

# Check the summary of the fitted model
summary(fit)

# Plot the residuals to check for any patterns
checkresiduals(fit)
names(fit)
pacf(fit$residuals)
############ Forecast the next 10 periods with the current model#########

h=10  #### enter the number of days to be forecasted
forecasted_values <- forecast(fit, h)

# Extract the last date from the xts object (data from Yahoo_finance website)
last_date <- index(Adj_new)[nrow(Adj_new)]

# Generate the next 10 dates as entered by user above
future_dates <- seq(last_date + 1, by = "days", length.out = h)

# Extract the forecasted mean values and converting them to numeric form
forecast_means <- as.numeric(forecasted_values$mean)

# Create a data frame with the future dates and forecasted values in dataframe format (table)
forecast_df <- data.frame(Date = future_dates, Forecast = forecast_means)

# Print the forecasted values in a tabular format
print(forecast_df)

################ Ploting the forecasted values to see trend ###############
ggplot(forecast_df, aes(x = Date, y = Forecast)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Forecasted Values", x = "Date", y = "Forecast") +
  theme_minimal()
###########################################################################

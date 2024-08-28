library(e1071)   # For SVM Model
library(openair) # for analyzing air quality (type) data
library(pdfetch) # For fetching data from Yahoo Finance
library(urca)    # for performing different tests
library(caret)   # for data splitting
library(ggplot2) # for plotting
library(xts)     # for working with xts objects
library(tseries) # for indexing date
library(forecast) # for forecasting future dates

# For cleaning Environment
#rm(list = ls())
# Importing data from Yahoo Finance using pdfetch
raw_data = pdfetch_YAHOO("^AORD")

# Storing values of adjclose column to variable
Adj = raw_data[,"^AORD.adjclose"]

# Checking missing values
missing_values = sum(is.na(Adj))
missing_indexes = which(is.na(Adj))
Adj_new = na.omit(Adj)
missing_indexes_1 = which(is.na(Adj_new))

#################################################
#Normalizing the data to reduce errors
normalize = function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}
Adj_new_norm = normalize(Adj_new)
#################################################

# Plotting original values
plot(Adj_new_norm, main = "Time Series Plot of ^AORD.adjclose", ylab = "^AORD.adjclose", xlab = "Date")

## Few test to check Nature of data (In this model not required)
#acf(Adj_new_norm)   # ACF test
#pacf(Adj_new_norm)  # PACF test
#adf.test(Adj_new_norm) # adf test

#########################################################################
# Applying SVM (Support Vector Machine)
##########################################################################

# Create lagged features from previous values of Data
create_lagged_features = function(data, lag = 1) {
  n = length(data)
  lagged_data = embed(data, lag + 1)
  x = lagged_data[, -1]
  y = lagged_data[, 1]
  return(list(x = x, y = y))
}

# Adjusing Lag values manually to get best results
lag = 15
lagged_features = create_lagged_features(Adj_new_norm, lag)
x = lagged_features$x
y = lagged_features$y   ## creating lagged values of date and prices.

# Split the data
set.seed(123) ## So that data remains same every time during spliting
train_divider = createDataPartition(y, p = 0.8, list = FALSE)


x_training = x[train_divider, ]
y_training = y[train_divider]
x_testing = x[-train_divider, ]
y_testing = y[-train_divider]

# Train the SVM model using "linear" kernel & eps-regression type
# Setting parameters manually
svm_model = svm(x_training, y_training, type = 'eps-regression', kernel = 'linear', tol = 0.01, epsilon = 0.1 )

# Make predictions
predictions_y = predict(svm_model, x_testing)

# Evaluate the model mean square error
Mse = mean((predictions_y - y_testing)^2)
print(paste("Mean Squared Error:", Mse))


######################################################
# Plot actual vs predicted values (test vs train) How good model is...

plot(y_testing, type = "l", col = "blue", main = "Actual vs Predicted", ylab = "^AORD.adjclose")
lines(predictions_y, col = "red")
legend("topright", legend = c("Actual", "Predicted"), col = c("blue", "red"), lty = 1)
########################################################
# Forecast future values:
forecast_days = 30  # Setting the forecasted days manually
last_price = tail(as.numeric(Adj_new), lag)
future_values_forecast = numeric(forecast_days)
#########################################################

## Applying loop to to run according to days entered to predict forecasted values

for (i in 1:forecast_days) {
  # Convert last_values to a matrix with a single row
  last_values_matrix = matrix(last_price, nrow = 1)
  
  # Predict the next value
  next_value = predict(svm_model, last_values_matrix)
  
  # Store the predicted value
  future_values_forecast[i] = next_value
  
  # Update last_values for the next iteration
  last_price = c(last_price[-1], next_value)
}
################################################################

# Combining future dates with forecasted values to make a table for understanding
last_date_data = index(Adj_new_norm)[length(Adj_new_norm)]
dates_of_future = seq(last_date_data + 1, by = "days", length.out = forecast_days)
prices_predicted = data.frame(Date = dates_of_future, Predicted_AdjClose = future_values_forecast)

print(prices_predicted) ## Printing forecasting values in console


# Plot predicted values with line and dots graphs showing behaviour

plot(prices_predicted$Date, prices_predicted$Predicted_AdjClose, type = "o", col = "red", 
     xlab = "Date", ylab = "Predicted Adjusted Close", main = "Future Predictions - SVM",
     pch = 16)

############################################################################

library(caret)
library(forecast)
library(neuralnet)

# Date should be in a format recognizable by R, such as "yyyy-mm-dd".
sales_data <- data.frame(
  Date = seq(as.Date("2023-01-01"), as.Date("2023-07-31"), by="1 month"),
  Sales = c(100, 150, 120, 180, 200, 170, 220)
)

# Function to create and train the predictive model with a Neural Network
build_sales_forecast_model <- function(data) {
  # Split data into training and testing sets (80/20 split)
  train_size <- floor(0.8 * nrow(data))
  train_data <- data[1:train_size, ]
  test_data <- data[(train_size + 1):nrow(data), ]
  
  # Neural Network model
  nn_formula <- as.formula("Sales ~ Date")
  model <- neuralnet(nn_formula, data = train_data, hidden = c(5, 3), linear.output = TRUE)

# Function to make predictions using the trained model
make_sales_forecast <- function(model, new_data) {
  # Use the trained model to make predictions on new data
  predictions <- predict(model, newdata = new_data)
  
  return(predictions)
}

# sales forecasting process
main_sales_forecasting <- function(sales_data) {
  # Date column to time series object
  ts_data <- ts(sales_data$Sales, frequency = 12, start = c(2023, 1))
  
  # Create and train model
  model_info <- build_sales_forecast_model(sales_data)
  model <- model_info$model
  
  # Make predictions 
  predictions <- make_sales_forecast(model, model_info$test_data)
  
  # Convert the predictions 
  ts_predictions <- ts(predictions, frequency = 12, start = c(2023, 5))
  
  # Print the forecasted sales values
  print("Forecasted Sales:")
  print(ts_predictions)
  
  # Plot the actual sales data and the forecasted values
  plot(ts_data, col = "blue", main = "Customer Sales Forecast", ylab = "Sales")
  lines(ts_predictions, col = "red")
  legend("topleft", legend = c("Actual Sales", "Forecasted Sales"), col = c("blue", "red"), lwd = 1)
}

# Run the main sales forecasting process
main_sales_forecasting(sales_data)

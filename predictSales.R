library(caret)
library(neuralnet)

# Function to split data into training and testing sets
split_train_test <- function(data, train_size = 0.8) {
  train_size <- floor(train_size * nrow(data))
  train_data <- data[1:train_size, ]
  test_data <- data[(train_size + 1):nrow(data), ]
  return(list(train_data = train_data, test_data = test_data))
}

# Function to build a neural network model
build_neural_net_model <- function(train_data, formula, hidden_layers = c(5, 3)) {
  model <- neuralnet(formula, data = train_data, hidden = hidden_layers, linear.output = TRUE)
  return(model)
}

# Function to make predictions using a trained model
make_predictions <- function(model, new_data) {
  predictions <- predict(model, newdata = new_data)
  return(predictions)
}

# Function to plot actual and forecasted sales data
plot_sales_forecast <- function(actual_sales, forecasted_sales) {
  plot(actual_sales, col = "blue", main = "Customer Sales Forecast", ylab = "Sales")
  lines(forecasted_sales, col = "red")
  legend("topleft", legend = c("Actual Sales", "Forecasted Sales"), col = c("blue", "red"), lwd = 1)
}

# Main forecasting process
main_sales_forecasting <- function(sales_data) {
  ts_data <- ts(sales_data$Sales, frequency = 12, start = c(2023, 1))
  formula <- as.formula("Sales ~ Date")

  split_data <- split_train_test(sales_data)
  train_data <- split_data$train_data
  test_data <- split_data$test_data

  model <- build_neural_net_model(train_data, formula)
  predictions <- make_predictions(model, test_data)

  ts_predictions <- ts(predictions, frequency = 12, start = c(2023, 5))

  print("Forecasted Sales:")
  print(ts_predictions)

  plot_sales_forecast(ts_data, ts_predictions)
}

# Run the main sales forecasting process
sales_data <- data.frame(
  Date = seq(as.Date("2023-01-01"), as.Date("2023-07-31"), by="1 month"),
  Sales = c(100, 150, 120, 180, 200, 170, 220)
)

main_sales_forecasting(sales_data)

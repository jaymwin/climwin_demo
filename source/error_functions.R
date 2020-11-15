
# Function that returns Mean Absolute Error
mae <- function(error) {
  
  mean(abs(error))
  
}

rmse <- function(error) {
  
  sqrt(mean(error ^ 2))
  
}

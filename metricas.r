
# Erro Quadrático (SE)
se <- function(y, y_hat) {
  stopifnot(length(y) == length(y_hat),
            is.numeric(y),
            is.numeric(y_hat))

  (y - y_hat) ^ 2
}

# Erro quadrático médio (MSE)
mse <- function(y, y_hat) mean(se(y, y_hat), na.rm = TRUE)

# Raiz do erro quadrático médio (RMSE)
rmse <- function(y, y_hat) sqrt(mse(y, y_hat))


mase_cal <- function(insample, outsample, forecasts){
  frq <- 1
  forecastsNaiveSD <- rep(NA,frq)
  for (j in (frq+1):length(insample)){
    forecastsNaiveSD <- c(forecastsNaiveSD, insample[j-frq])
  }
  masep<-mean(abs(insample-forecastsNaiveSD),na.rm = TRUE)
  
  outsample <- as.numeric(outsample) ; forecasts <- as.numeric(forecasts)
  
  mase <- mean((abs(outsample-forecasts)))/masep
  
  return(mase)
}

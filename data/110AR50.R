set.seed(727850)

time_series <- list()
i <- 1
while(i<1000){
  time_series[[i]] <- arima.sim(list(order = c(1,1,0), ar = 0.7), n = 50)
  i <- i + 1
}

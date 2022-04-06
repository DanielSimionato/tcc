set.seed(727850)

time_series <- list()
i <- 1
while(i<1000){
  time_series[[i]] <- arima.sim(list(order = c(2,1,0), ar = c(0.7,-0.7)), n = 1000)
  i <- i + 1
}


get_y <- function(test, form) model.response(model.frame(form, test, na.action = NULL))

RF <- function(form,train,test) {
    m <- ranger(formula = form, data=train, num.trees = 200)
    predict(m,test)$predictions
  }

RF_loss <- function(train, test, form, avg=TRUE) {
    y <- get_y(test, form)
    y_hat <- RF(form, train, test)
    mse(y, y_hat)
  }

MODELOARIMA <- function(form,tstrain,tstest) {
  m <- auto.arima(tstrain)
  as.vector(forecast(m,h = length(tstest))$mean)
}

ARIMA_loss <- function(train, test, form, avg=TRUE) {
  tstrain <- ts(train$target)
  tstest <- ts(test$target)
  y <- get_y(test, form)
  y_hat <- MODELOARIMA(form,tstrain, tstest)
  mse(y, y_hat)
}

MODELOETS <- function(form,tstrain,tstest) {
  m <- ets(tstrain)
  as.vector(forecast(m,h = length(tstest))$mean)
}

ETS_loss <- function(train, test, form, avg=TRUE) {
  tstrain <- ts(train$target)
  tstest <- ts(test$target)
  y <- get_y(test, form)
  y_hat <- MODELOETS(form,tstrain, tstest)
  mse(y, y_hat)
}

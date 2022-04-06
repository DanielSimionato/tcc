
partition <- function(x, hat.ratio) {
  len <- NROW(x)
  
  if (class(x)[1] == "data.frame") {
    train <- x[ seq_len(hat.ratio * len),]
    test <-  x[-seq_len(hat.ratio * len),]
  } else {
    train <- x[ seq_len(hat.ratio * len)]
    test <-  x[-seq_len(hat.ratio * len)]
  }
  list(train = train, test = test)
}

# Embaralha os dados
cv.shuffle <- function(x) x[sample(NROW(x)), ]

# Cria os k folds
cv.folds <- function(x, nfolds) {
  cut(seq_len(NROW(x)), breaks = nfolds, labels = FALSE)
}

# out-of-time validation
oot <- function(x, FUN, ...){
  cv.res <- list()
  
  xp <- partition(x,0.7)
  train <- xp$train
  test <- xp$test
  
  cv.res <- FUN(train, test, ...)
  cv.res
}


# k-fold cross validation
kf_xval <- function(x, nfolds, FUN, shuffle.rows = TRUE, average_results = TRUE, ...) {
  if (shuffle.rows) x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list() 
  for (i in seq_len(nfolds)) {
    ts.id <- which(f == i)

    train <- x[-ts.id, ]
    test  <- x[ ts.id, ]
    cv.res[[i]] <- FUN(train, test, ...)
  }
  
  if(average_results) {
    cv.res <- mean(unlist(cv.res, use.names = FALSE))
  }
  cv.res
}


# Blocked k-fold cross validation
blocked_kf_xval <- function(x, nfolds, FUN, ...) {
  kf_xval(x = x, nfolds = nfolds, FUN = FUN, shuffle.rows = FALSE, ...)
}


# Modified k-fold Cross Validation
modified_xval <- function(x, nfolds, FUN, average_results = TRUE, ...) {
  K <- floor(sqrt(ncol(x))) #inventei aqui
  x$aux <- seq_len(nrow(x))
  x <- cv.shuffle(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  for (i in seq_len(nfolds)) {
    ts.id <- which(f == i)
    test  <- x[ts.id, ]

    depRows <- unique(unlist(lapply(test$aux, function(z) (z-K-1):(z+K-1L))))
    depRows <- depRows[depRows > 0]

    train <- x[-c(ts.id, depRows), ]
    if (nrow(train) < 1) {
      cv.res[[i]] <- NA #rep(NA_real_, times = 3)
    } else {
      test$aux <- NULL
      train$aux <- NULL

      cv.res[[i]] <- FUN(train, test, ...)
    }
  }
  
  if(average_results) {
    cv.res <- mean(unlist(cv.res), na.rm = TRUE)
  }
  cv.res
}


# hv - block Cross Validation
hv.block_xval <- function(x, nfolds, FUN, average_results, ...) {
  K <- ncol(x)
  f <- cv.folds(x, nfolds)

  cv.res <- list()
  seq. <- seq_len(nfolds)
  lseq <- seq.[length(seq.)]
  for (i in seq.) {
    ts.id <- which(f == i)
    # upper cut
    kcuts <- integer(0L)
    if (i + 1L <= lseq) {
      upper.fold <- which(f == i + 1L)
      upper.cut <- upper.fold[1:K]
      if (any(is.na(upper.cut))) {
        upper.cut <- upper.cut[!is.na(upper.cut)]
      }
      
      kcuts <- c(kcuts, upper.cut)
    }
    # lower cut
    if (i - 1L >= 1L) {
      lower.fold <- which(f == i - 1L)
      len.lf <- length(lower.fold)
      idx <- (len.lf - K + 1L):len.lf
      idx <- idx[idx > 0]
      lower.cut <- lower.fold[idx]
      kcuts <- c(kcuts, lower.cut)
    }

    train <- x[-c(ts.id, kcuts), ]
    test  <- x[ts.id, ]

    cv.res[[i]] <- FUN(train, test, ...)
  }
  
  if (average_results) {
    cv.res <- mean(unlist(cv.res), na.rm = TRUE)
  }
  cv.res
}


# Markov Cross Validation
markov_xval <- function(x, nfolds, FUN, average_results, ...){
  cv.res <- list()
  
  p <- 1
  while (abs(pacf(ts(x$target),plot=F)$acf[p+1])>2*1/sqrt(nrow(x))) {p <- p + 1}
  
  if (p%%3==0){m <- 2*p/3}else{m <- 2*floor(p/3)+2}
  
  d <- sample(c(1,-1), size=2,replace=T)
  for (t in 3:nrow(x)){
    if ((d[t-1]<0)&(d[t-2]<0)) {
      d[t] <- 1
    } else if ((d[t-1]>0)&(d[t-2]>0)) {
      d[t] <- -1
    } else if (runif(1,0,1)>0.5) {
      d[t] <- -1
    } else {
      d[t] <- 1
    }
  }
  
  Id <- c()
  for (t in 1:length(d)) {
    Id[t] <- t%%m + 1 + if(d[t]>0){m}else{0}
  }
  
  for (u in 1:2*m){
    contador <- 1
    for (t in 1:nrow(x)) {
      if (Id[t]==u) { 
        if (contador%%2==1) {
          Id[t] <- -u
        }
        contador <- contador + 1
      }
    }
    train <- x[Id==-u,]
    test  <- x[Id==u,]
    
    cv.res[[u]] <- FUN(train, test, ...)
  }
  
  if (average_results) {
    cv.res <- mean(unlist(cv.res), na.rm = TRUE)
  }
  cv.res
}

estimate_k <-
  function(x, m.max=20,tol=.15) {
    require(tseriesChaos)
    
    fn.out <- false.nearest(x, m.max, d=1, t=1)
    fn.out <- round(fn.out,4)
    fn.out[is.na(fn.out)] <- 0
    
    fnp.tol <- fn.out["fraction",] > tol
    fnp.tol.sum <- sum(fnp.tol)
    
    m <- ifelse(fnp.tol.sum < m.max,fnp.tol.sum + 1, m.max)
    m
  }

performance_estimation <- function(train, form, pred_model, nfolds) {
    cat("Estimando a perda usando ...\n")
    
    cat("> Out-of-Sample \n")
    oot <- oot(x = train,
               FUN = pred_model,
               form = form)
    
    cat("> K-fold cross validation \n")
    std_x_val <- kf_xval(x = train,
                         nfolds = nfolds,
                         FUN = pred_model,
                         shuffle.rows = TRUE,
                         form = form, 
                         average_results = TRUE)
    
    cat("> Modified cross-validation \n")
    mod_x_val <- modified_xval(x = train,
                               nfolds = nfolds,
                               FUN = pred_model,
                               average_results = TRUE,
                               form = form)
    
    cat("> Blocked cross-validation \n")
    blocked_x_val <- blocked_kf_xval(x = train,
                                     nfolds = nfolds,
                                     FUN = pred_model,
                                     form = form)
    
    cat("> HV-Blocked cross-validation \n")
    hvb_x_val <- hv.block_xval(x = train,
                               nfolds = nfolds,
                               FUN = pred_model,
                               average_results = TRUE,
                               form = form)
    
    cat("> Markov cross-validation \n")
    mkv_x_val <- markov_xval(x = train,
                             nfolds = nfolds,
                             FUN = pred_model,
                             average_results = TRUE,
                             form = form)
    
    loss_estimations <- c(oot = oot,
                          x_val = std_x_val,
                          m_x_val = mod_x_val,
                          b_x_val = blocked_x_val,
                          hvb_x_val = hvb_x_val,
                          mkv_x_val = mkv_x_val)
    loss_estimations
  }


workflow <- function(ds,form,predictive_algorithm,nfolds,outer_split,is_embedded){
    
    if(!is_embedded){
      nd <- ndiffs(ds)
      if (nd > 0) { ds <- diff(ds, differences=nd) }
      khat <- estimate_k(ds[1:(length(ds) * outer_split)], m.max = 30, tol = .01)
      if (khat < 8) { khat <- 8 }
      x <- embed_timeseries(as.numeric(ds), khat+1)
    } else {
      x<-ds
    }
    
    xp <- partition(x, outer_split)
    train <- xp$train
    test <- xp$test
    
    pred_model <- switch(predictive_algorithm,
                         'ARIMA' = ARIMA_loss,
                         'ETS' = ETS_loss,
                         'RF' = RF_loss,
                         ARIMA_loss)
    
    true_loss <- pred_model(train = train,
                            test = test,
                            form = form)
    
    estimated_loss <- performance_estimation(train = train,
                                             form = form,
                                             pred_model = pred_model,
                                             nfolds = nfolds)
    
    err_estimation <- sapply(estimated_loss,
                             function(u) { ((u - true_loss) / true_loss) * 100 })
    
    list(err_estimation=err_estimation, 
         est_err=estimated_loss,
         err=true_loss)
  }

rankcompare <- function(results) {
  err_estimation <- lapply(results,function(x) {
    tryCatch(x$err_estimation,
             error =function(e) {NULL})})
  err_estimation <- err_estimation[!sapply(err_estimation, is.null)]
  fr <- do.call(rbind, err_estimation)
  fr <- as.data.frame(fr)
  rownames(fr) <- NULL
  colnames(fr) <- c("OOT", "CV","ModCV", "BCV","hvBCV","M-CV")
  fr_abs <- abs(fr)
  fr_abs_rank <- apply(fr_abs, 1, rank)
  avg = rowMeans(fr_abs_rank)
  sdev = apply(fr_abs_rank,1, sd)
  ord0 <- order(avg)
  ord <- names(sort(avg))
  methods <- names(avg)
  ds <- data.frame(avg=avg,sdev=sdev, methods=methods, row.names = NULL)
  ds$methods <- factor(ds$methods, levels = ord)
  return(ds)
}
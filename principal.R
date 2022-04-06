rm(list = ls())

set.seed(727850)

if(!require(tsensembler)) install.packages("tsensembler");require(tsensembler)
if(!require(parallel)) install.packages("parallel");require(parallel)
if(!require(forecast)) install.packages("forecast");require(forecast)
if(!require(reshape2)) install.packages("reshape2");require(reshape2)
if(!require(ggplot2)) install.packages("ggplot2");require(ggplot2)
if(!require(ranger)) install.packages("ranger");require(ranger)

source("C:/Users/55119/Desktop/scripts/TCC/validacao_cruzada.r")
source("C:/Users/55119/Desktop/scripts/TCC/workflows.r")
source("C:/Users/55119/Desktop/scripts/TCC/metricas.r")
source("C:/Users/55119/Desktop/scripts/TCC/modelos.r")

modelos <- c('ETS','ARIMA')

dados <- c('100AR50','100AR100','100AR1000','200AR1000',
           '110AR50','110AR100','110AR1000','210AR1000',
           '101ARMA1000','111ARIMA1000','covid')


for (jj in 1:length(modelos)){
  for (ii in 1:length(dados)) {
    source(sprintf("C:/Users/55119/Desktop/scripts/TCC/data/%s.r",dados[ii]))
    
    final_results <- mclapply(1:length(time_series),function(i) {
      cat("\n\n",i,"\n")
      ds <- time_series[[i]]
      
      x <- workflow(ds = ds,
                    form = target ~ .,
                    predictive_algorithm = sprintf('%s',modelos[jj]),
                    nfolds = 10,
                    outer_split = 0.8, 
                    is_embedded = FALSE)
      x }, mc.cores = 1)
    
    
    p <- ggplot(data = rankcompare(final_results),aes(x = methods,y = avg)) +
      geom_bar(stat="identity",fill='steelblue') +
      theme_minimal() +
      theme(axis.text.x  = element_text(angle = 35,size = 14,hjust = 1)) +
      theme(axis.text.y  = element_text(size = 12),
            axis.title.y = element_text(size = 12)) +
      geom_errorbar(aes(ymin = avg - sdev,ymax = avg + sdev),width = .5,
                    position = position_dodge(.9)) +
      labs(x="",y="Ranking Medio",title = "")
    ggsave(p, file=sprintf("C:/Users/55119/Desktop/scripts/TCC/imagens/%s/%s.png",
                           modelos[jj],dados[ii]))
    
    cat("Limpando... \n\n")
    
    rm(time_series,final_results,p)
  }
}

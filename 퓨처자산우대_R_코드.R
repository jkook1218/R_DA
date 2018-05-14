setwd("C:/Users/JINKOOK/Desktop/미래에셋")
analysis_1 <- read.csv("고가만.csv")
analysis_plot <- analysis_1[,-1]
for_analysis <- function(x){
  x=48
  k=1
  j=44
  result_matrix <- matrix(nrow = x+1, ncol = 120)
  predict_matrix <- matrix(nrow = x+1, ncol = 120)
  for(j in 1:120){
    analysis_data_frame <- data.frame(analysis_plot[,j])
    row_num = nrow(analysis_data_frame)
    NonNAindex <- which(!is.na(analysis_data_frame))
    firstNonNA <- min(NonNAindex)
    repeat_num = row_num-(firstNonNA-1)
    if(repeat_num >= 77){
      repeat_num = 77
    }
    year = firstNonNA%/%12
    month = firstNonNA%%12
    analysis_data_frame <- na.omit(analysis_data_frame)
    if(firstNonNA >= 156){
      if(repeat_num < 30){
        k=1
        for(k in 1:repeat_num){
          result_matrix[48+k-repeat_num,j] <- 0
          predict_matrix[48+k-repeat_num,j] <- 0
          
        }
        
      }
      else{
        k=30
        for(k in 30:repeat_num){
          analysis_extract_data <- analysis_data_frame[1:k,]
          analysis_extract_data <- data.frame(analysis_extract_data)
          present_value <- analysis_extract_data[k,]
          analysis_trade <- ts(analysis_extract_data,start=c(2000+year,month),frequency=12)
          fit <- auto.arima(log(analysis_trade))
          var_fit <- arimaorder(fit)
          auto_fit <- as.matrix(var_fit)
          if(nrow(auto_fit) <= 3){
            fit <- arima(log(analysis_trade),method="CSS", c(auto_fit[1,1], auto_fit[2,1], auto_fit[3,1]))
            pred <- predict(fit, n.ahead = 1*1)
            result_return <- 2.718^pred$pred
            plus_percent <- result_return - present_value
            
            result_matrix[x-(repeat_num-k),j] <- result_return
            predict_matrix[x-(repeat_num-k),j] <- plus_percent
          }
          else {
            
            fit <- arima(log(analysis_trade), method="CSS",c(auto_fit[1,1], auto_fit[2,1], auto_fit[3,1]), 
                         seasonal = list(order = c(auto_fit[4,1], auto_fit[5,1], auto_fit[6,1]),period = 12))
            pred <- predict(fit, n.ahead = 1*1)
            result_return <- 2.718^pred$pred
            plus_percent <- result_return - present_value
            
            result_matrix[x-(repeat_num-k),j] <- result_return
            predict_matrix[x-(repeat_num-k),j] <- plus_percent
          }
          
        }
      }
    }
    else {
      k=1
      for(k in 1:(repeat_num-29)){
        
        analysis_extract_data <- analysis_data_frame[1:(156-firstNonNA+k),]
        analysis_extract_data <- data.frame(analysis_extract_data)
        present_value <- analysis_extract_data[(156-firstNonNA+k),]
        analysis_trade <- ts(analysis_extract_data,start=c(2000+year,month),frequency=12)
        fit <- auto.arima(log(analysis_trade))
        var_fit <- arimaorder(fit)
        auto_fit <- as.matrix(var_fit)
        if(nrow(auto_fit) <= 3){
          fit <- arima(log(analysis_trade),method="CSS", c(auto_fit[1,1], auto_fit[2,1], auto_fit[3,1]))
          pred <- predict(fit, n.ahead = 1*1)
          result_return <- 2.718^pred$pred
          plus_percent <- result_return - present_value
          
          result_matrix[48+k-(repeat_num-29),j] <- result_return
          predict_matrix[48+k-(repeat_num-29),j] <- plus_percent
        }
        else {
          
          fit <- arima(log(analysis_trade),method="CSS", c(auto_fit[1,1], auto_fit[2,1], auto_fit[3,1]), 
                       seasonal = list(order = c(auto_fit[4,1], auto_fit[5,1], auto_fit[6,1]),period = 12))
          pred <- predict(fit, n.ahead = 1*1)
          result_return <- 2.718^pred$pred
          plus_percent <- result_return - present_value
          
          result_matrix[48+k-(repeat_num-29),j] <- result_return
          predict_matrix[48+k-(repeat_num-29),j] <- plus_percent
        }
        
      }
    }
    
  }
  
  write.csv(result_matrix, "result_high.csv")
  write.csv(predict_matrix, "predict_high.csv")
}



setwd("C:/Users/JINKOOK/Desktop/미래에셋")
for_predict <- function(x){
  pred = read.csv('result.csv')[-49,-1]
  real = read.csv('종가만.csv')[,-1]
  
  percent_matrix <- matrix(nrow = 48, ncol = 120)
  top5_matrix <- c()
  i=1
  for(i in 1:120){
    pred_price <- pred[,i]
    real_price = real[,i]
    pred_price <- data.frame(pred_price)
    real_price <- data.frame(real_price)
    pred_price <- na.omit(pred_price)
    real_price <- na.omit(real_price)
    num_pred <- nrow(pred_price)
    num_real <- nrow(real_price)
    
    j=1
    for(j in 1:num_pred){
      if(pred_price[j,]==0){
        percent = 0
      }
      else{
        percent <- as.numeric(pred_price[j,])/as.numeric(real_price[(num_real-num_pred),])
      }
      percent_matrix[(48-num_pred+j),i] <- percent
    }
  }
  

  k=1
  for(k in 1:48){
    top5 <- head(sort(percent_matrix[k,],decreasing=TRUE,index.return = TRUE)$ix,5)
    top5 = colnames(pred)[top5]
    top5_matrix =  rbind(top5_matrix, matrix(top5,nrow = 1,ncol=5))
  }
  
  write.csv(top5_matrix, "5가지기업_code.csv")
  
  write.csv(percent_matrix, "최종수익률.csv")
}

for_predict()



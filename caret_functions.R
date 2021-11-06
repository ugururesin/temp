# Function for splitting dataset
partition <- function(data, factor, trainsize = configs$trainset_ratio){
  if (("package:caret" %in% search()) == FALSE){
    stop("Install and Load 'caret' package")
  }
  if (is.null(factor)){
    train.index <- createDataPartition(as.numeric(row.names(data)),
                                       times = 1, p = trainsize, list = FALSE)
    train <- data[train.index, ]
    test <- data[-train.index, ]
  }
  else{
    train.index <- createDataPartition(factor,
                                       times = 1, p = trainsize, list = FALSE)
    train <- data[train.index, ]
    test <- data[-train.index, ]
  }
  partitioned.data <- list(train = train, test = test)
  return(partitioned.data)
}

# Function for training
train_model <- function(data,method,replication,tunelength,show,msg){
  
  controller <- trainControl(method="repeatedcv", repeats=replication,
                             verboseIter=show, returnData=FALSE, trim=TRUE)
  
  cat(msg)
  fit_model <- train(Y ~ ., data=data, method=method,
                     trControl=controller, preProcess=c("center","scale"),
                     tuneLength=tunelength)
  return(fit_model)
}

# Function for accuracy calculation
get_accuracy <- function(model,test) {
  acc_tra <- max(model$results$Accuracy)
  pre_tst <- predict(model, newdata=test)
  cfm_tst <- confusionMatrix(pre_tst, test$Y)
  acc_tst <- cfm_tst$overall[1]
  return(c(acc_tra, acc_tst))
}
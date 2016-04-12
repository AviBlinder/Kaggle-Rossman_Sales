tra<-train[,feature_names]
RMPSE<- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    elab<-exp(as.numeric(labels))-1
    epreds<-exp(as.numeric(preds))-1
    err <- sqrt(mean((epreds/elab-1)^2))
    return(list(metric = "RMPSE", value = err))
}
nrow(train);ncol(train)
h<-sample(nrow(train),xgboost_sampble_size)
library(xgboost)
dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Sales+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Sales+1)[-h])
rm(tra)


watchlist<-list(val=dval,train=dtrain)

param <- list(  objective = "reg:linear", 
                booster   = "gbtree",
                eta              = xgboost_eta , 
                max_depth        = xgboost_max_depth
#                subsample        = xgboost_subsamble, 
#                colsample_bytree = xgboost_colsample_bytree
                #num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
                #booster   = "gblinear",
)

clf <-    xgb.train(   params              = param, 
                       data                = dtrain, 
                       nrounds             = xgboost_nrounds, 
                       verbose             = 2,
#                      early.stop.round    = 30,
                       watchlist           = watchlist,
                       maximize            = FALSE,
                       feval=RMPSE
)
gc()
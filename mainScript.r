rm(list=ls())
gc()

Sys.setlocale("LC_ALL", "C")

setwd("/Users/aviblinder/Documents/Avi/MOOC/Data Science/Kaggle/Rossman Store Sales/")

set.seed(178) 
xgboost_sampble_size <- 50
xgboost_eta          <- 0.05
xgboost_max_depth    <- 230
xgboost_subsamble    <- 0.7
xgboost_colsample_bytree <- 0.6
xgboost_nrounds         <- 1200

source("Scripts_local//preprocess.r")
dim(train)
source("Scripts_local/xgboost_model.r")


pred1 <- exp(predict(clf, data.matrix(test[,feature_names]))) -1
submission <- data.frame(Id=test$Id, Sales=pred1)
cat("saving the submission file\n")
write.csv(submission, "./Submissions/xgboost050.csv",row.names=FALSE)
head(submission)


#######
ens_copy <- read.csv("./Submissions/ensamble3 - copy.csv")
ens <- read.csv("./Submissions/ensamble3.csv")
head(ens)
pred1 <- ens$Sales
head(pred1)

pred1_adjusted <- pred1[(test$Month_Num8==1 & test$DayOfWeek==5 & test$DOM >= 14 |
                             test$Month_Num9==1 & test$DayOfWeek==5 & test$DOM == 11)]
pred1_adjusted <- pred1_adjusted*1.05 

pred1[(test$Month_Num8==1 & test$DayOfWeek==5 & test$DOM >= 14 |
        test$Month_Num9==1 & test$DayOfWeek==5 & test$DOM == 11)]  <- pred1_adjusted 

head(submission$Sales[c(30,43,44,66,73,84)])
head(pred1[c(30,43,44,66,73,84)])

submission$Sales <- pred1
cat("saving the submission file\n")
write.csv(submission, "./Submissions/ensamble3_adjusted1.csv",row.names=FALSE)
head(submission)

###########################
test <- read.csv(unz("./Data/test.csv.zip", "test.csv"),header = T,stringsAsFactor=T)

pred002 <- read.csv("./Submissions/xgboost1.csv")
head(pred002)
pred010 <- read.csv("./Submissions/xgboost010.csv")
head(pred010)
pred025 <- read.csv("./Submissions/xgboost025.csv")

predrfHex_500 <- read.csv("./Submissions/h2o_rf_500trees.csv")
head(predrfHex_500)

whg <- 1/3

submission <-  data.frame(Id=test$Id, Sales=(pred002$Sales*whg + pred010$Sales*whg + 
                                    pred025$Sales*whg ))
head(submission)
write.csv(submission, "./Submissions/ensamble4.csv",row.names=FALSE)


library(xgboost)
xgboost::xgb.importance(clf)

###### Ensamble
rm(list=ls())
gc()

Sys.setlocale("LC_ALL", "C")

setwd("/Users/aviblinder/Documents/Avi/MOOC/Data Science/Kaggle/Rossman Store Sales/")

test <- read.csv(unz("./Data/test.csv.zip", "test.csv"),header = T,stringsAsFactor=T)


w1 <- 0.9
w2 <- 0.1
w1 + w2

pred0025a <- read.csv("./Submissions//xgboost_submission0025_nr7000.csv")
head(pred0025a)

pred0025b <- read.csv("./Submissions/xgboost_submission0025_nr10000.csv")
head(pred0025b)

pred0025c <- read.csv("./Submissions/xgboost_submission0025_nr11000_mdepth18.csv")
head(pred0025c)

submission <-  data.frame(Id=test$Id, Sales=(pred0025a$Sales + pred0025a$Sales+ pred0025a$Sales)/3)

head(pred0025a$Sales)
head(pred0025b$Sales)
head(pred0025c$Sales)
head(submission$Sales)
write.csv(submission, "./Submissions/ensemble3_25a__25b_25c.csv",row.names=FALSE)


diffs <- pred0025a$Sales - submission$Sales
hist(diffs)
summary(diffs)
table(diffs)

diffs <-pred002$Sales - pred001$Sales
head(diffs)
hist(diffs,breaks=100)
mean(diffs)
summary(diffs)
mean(diffs);sd(diffs)

low_b <- mean(diffs) + sd(diffs)*2
upp_b <- mean(diffs) - sd(diffs)*2

test_diffs <- test[(diffs < upp_b | diffs > low_b),]

round(prop.table(table(test_diffs$Store)),2)
t1 <- as.data.frame(prop.table(table(test_diffs$DayOfWeek)))
t1[t1$Freq > 0]
head(t1)
which.max(table(test_diffs$Store))
t1

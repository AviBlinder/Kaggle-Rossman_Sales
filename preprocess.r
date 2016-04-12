### aggregate mean of Sales by different Competition Distances and Competition Open-Since

##to do: calculate difftime between Date and Promo2SinceDate
#rm(list=ls())
library(caret)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(lubridate)
library(reshape2)

setwd("/Users/aviblinder/Documents/Avi/MOOC/Data Science/Kaggle/Rossman Store Sales/")

cat("loading datasets \n")
train <- read.csv(unz("./Data/train.csv.zip", "train.csv"),header = T,stringsAsFactor=T)
test <- read.csv(unz("./Data/test.csv.zip", "test.csv"),header = T,stringsAsFactor=T)
store <- read.csv(unz("./Data/store.csv.zip", "store.csv"),header = T,stringsAsFactor=T)
states_old <- read.csv("./Data/store_states.csv")
states <- read.csv("./Data/store_states_calc_info.csv")
##################################################################################################
states <- merge(states_old,states,by="State")
store <- merge(store,states,by = "Store")

train$Store <- factor(train$Store)
test$Store <- factor(test$Store)
store$Store <- factor(store$Store)
##################################################################################################
##Add statistics of stores based on Sales from train
cat("aggregating Sales by Store - Sum \n")
agg1 <- aggregate(Sales ~ Store,data=train,FUN=function(x) sum(x)/1000)
store$Sales_sum <- agg1$Sales

cat("aggregating Sales by Store - Mean \n")
agg2 <- aggregate(Sales ~ Store,data=train,FUN=mean,na.rm=TRUE)
store$Sales_mean <- agg2$Sales
store$Sales_mean_round <- round(store$Sales_mean / 100)

cat("aggregating Sales by Store - Median  \n")
agg_medSales <- aggregate(Sales ~ Store,data=train,FUN=median,na.rm=TRUE)
store$Sales_median <- agg_medSales$Sales
store$Sales_median_round <- round(store$Sales_median / 100)

cat("aggregating Sales by Month and Store \n")
agg_Sales_by_Month <- aggregate(Sales ~ month(as.Date(Date)) + Store,data=train,FUN=mean,na.rm=TRUE)
names(agg_Sales_by_Month) <- c("Month","Store","Sales_Mean_by_Month")
dcast_agg_Sales_by_Month <- dcast(agg_Sales_by_Month,Store ~ Month,value.var = "Sales_Mean_by_Month")
names(dcast_agg_Sales_by_Month) <- c("Store","MeanSalesMonth1","MeanSalesMonth2",
                                     "MeanSalesMonth3","MeanSalesMonth4","MeanSalesMonth5",
                                     "MeanSalesMonth6","MeanSalesMonth7","MeanSalesMonth8",
                                     "MeanSalesMonth9","MeanSalesMonth10","MeanSalesMonth11",
                                     "MeanSalesMonth12")
head(dcast_agg_Sales_by_Month)
store <- merge(store,dcast_agg_Sales_by_Month,by="Store")
#####
cat("aggregating Sales by Month and Store  - Rounded \n")
agg_Sales_by_Month_R <- agg_Sales_by_Month
head(agg_Sales_by_Month_R)

agg_Sales_by_Month_R$Sales <- round(agg_Sales_by_Month_R$Sales / 100)
head(agg_Sales_by_Month_R)
names(agg_Sales_by_Month_R) <- c("Month","Store","Sales_Mean_by_Month_Rounded")

dcast_agg_Sales_by_Month_R <- dcast(agg_Sales_by_Month_R,Store ~ Month,value.var = "Sales_Mean_by_Month_Rounded")
names(dcast_agg_Sales_by_Month_R) <- c("Store","MeanSalesRoundedMonth1",
                                       "MeanSalesRoundedMonth2",
                                       "MeanSalesRoundedMonth3",
                                       "MeanSalesRoundedMonth4",
                                       "MeanSalesRoundedMonth5",
                                       "MeanSalesRoundedMonth6",
                                       "MeanSalesRoundedMonth7",
                                       "MeanSalesRoundedMonth8",
                                       "MeanSalesRoundedMonth9",
                                       "MeanSalesRoundedMonth10",
                                       "MeanSalesRoundedMonth11",
                                       "MeanSalesRoundedMonth12")
head(dcast_agg_Sales_by_Month_R)
store <- merge(store,dcast_agg_Sales_by_Month_R,by="Store")

#######
cat("aggregating Sales by DayOfWeek and Store \n")
agg_Sales_by_DayOfWeek <- aggregate(Sales ~ DayOfWeek + Store,data=train,FUN=mean,na.rm=TRUE)
head(agg_Sales_by_DayOfWeek,10)

names(agg_Sales_by_DayOfWeek) <- c("DayOfWeek","Store","Sales_Mean_by_DOW")

dcast_agg_Sales_by_DayOfWeek <- dcast(agg_Sales_by_DayOfWeek,Store ~ DayOfWeek,value.var = "Sales_Mean_by_DOW")
head(dcast_agg_Sales_by_DayOfWeek)

names(dcast_agg_Sales_by_DayOfWeek) <- c("Store",
                                       "MeanSalesDOW1",
                                       "MeanSalesDOW2",
                                       "MeanSalesDOW3",
                                       "MeanSalesDOW4",
                                       "MeanSalesDOW5",
                                       "MeanSalesDOW6",
                                       "MeanSalesDOW7")
store <- merge(store,dcast_agg_Sales_by_DayOfWeek,by="Store")
####
cat("aggregating Sales by DayOfWeek and Store - Rounded \n")
agg_Sales_by_DayOfWeek_R <- agg_Sales_by_DayOfWeek
head(agg_Sales_by_DayOfWeek_R)

agg_Sales_by_DayOfWeek_R$Sales_Mean_by_DOW <- round(agg_Sales_by_DayOfWeek_R$Sales_Mean_by_DOW/100)
names(agg_Sales_by_DayOfWeek_R) <- c("DayOfWeek","Store","Sales_Mean_by_DOW_Rounded")

dcast_agg_Sales_by_DayOfWeek_R <- dcast(agg_Sales_by_DayOfWeek_R,Store ~ DayOfWeek,
                                        value.var="Sales_Mean_by_DOW_Rounded")
head(dcast_agg_Sales_by_DayOfWeek_R)

names(dcast_agg_Sales_by_DayOfWeek) <- c("Store",
                                         "MeanSalesRoundedDOW1",
                                         "MeanSalesRoundedDOW2",
                                         "MeanSalesRoundedDOW3",
                                         "MeanSalesRoundedDOW4",
                                         "MeanSalesRoundedDOW5",
                                         "MeanSalesRoundedDOW6",
                                         "MeanSalesRoundedDOW7")
store <- merge(store,dcast_agg_Sales_by_DayOfWeek,by="Store")

#######
cat("aggregating Sales by Promo and Store \n")
agg_Sales_by_Promo <- aggregate(Sales ~ Promo + Store,data=train,FUN=mean,na.rm=TRUE)
head(agg_Sales_by_Promo,10)

names(agg_Sales_by_Promo) <- c("Promo","Store","Sales_Mean_by_Promo")

dcast_agg_Sales_by_Promo <- dcast(agg_Sales_by_Promo,Store ~ Promo,value.var = "Sales_Mean_by_Promo")
head(dcast_agg_Sales_by_Promo)

names(dcast_agg_Sales_by_Promo) <- c("Store",
                                         "PromoOff",
                                         "PromoOn")
store <- merge(store,dcast_agg_Sales_by_Promo,by="Store")

######
#StateHoliday
cat("aggregating Sales by StateHoliday and Store \n")
agg_Sales_by_StateHoliday <- aggregate(Sales ~ StateHoliday + Store,data=train,FUN=mean,na.rm=TRUE)
head(agg_Sales_by_StateHoliday,10)

names(agg_Sales_by_StateHoliday) <- c("StateHoliday","Store","Sales_Mean_by_StateHoliday")

dcast_agg_Sales_by_StateHoliday <- dcast(agg_Sales_by_StateHoliday,Store ~ StateHoliday,
                                         value.var = "Sales_Mean_by_StateHoliday")
head(dcast_agg_Sales_by_StateHoliday)

names(dcast_agg_Sales_by_StateHoliday) <- c("Store",
                                     "StateHoliday0",
                                     "StateHolidaya",
                                     "StateHolidayb",
                                     "StateHolidayc")
store <- merge(store,dcast_agg_Sales_by_StateHoliday,by="Store")

######
#SchoolHoliday
cat("aggregating Sales by SchoolHoliday and Store \n")
agg_Sales_by_SchoolHoliday <- aggregate(Sales ~ SchoolHoliday + Store,data=train,FUN=mean,na.rm=TRUE)
head(agg_Sales_by_SchoolHoliday,10)

names(agg_Sales_by_SchoolHoliday) <- c("SchoolHoliday","Store","Sales_Mean_by_SchoolHoliday")

dcast_agg_Sales_by_SchoolHoliday <- dcast(agg_Sales_by_SchoolHoliday,Store ~ SchoolHoliday,
                                          value.var = "Sales_Mean_by_SchoolHoliday")
head(dcast_agg_Sales_by_SchoolHoliday)

names(dcast_agg_Sales_by_SchoolHoliday) <- c("Store",
                                             "SchoolHoliday0",
                                             "SchoolHoliday1")
store <- merge(store,dcast_agg_Sales_by_SchoolHoliday,by="Store")

#######
cat("Creating ranks of Stores by Sales_mean_round")
store$Rank1 <- ifelse(store$Sales_mean_round > 0  & store$Sales_mean_round <= 30 ,1,0)
store$Rank2 <- ifelse(store$Sales_mean_round > 30 & store$Sales_mean_round <= 35 ,1,0)
store$Rank3 <- ifelse(store$Sales_mean_round > 35 & store$Sales_mean_round <= 38 ,1,0)
store$Rank4 <- ifelse(store$Sales_mean_round > 38 & store$Sales_mean_round <= 40 ,1,0)
store$Rank5 <- ifelse(store$Sales_mean_round > 40 & store$Sales_mean_round <= 42 ,1,0)
store$Rank5A <- ifelse(store$Sales_mean_round > 42 & store$Sales_mean_round <= 44 ,1,0)
store$Rank6 <- ifelse(store$Sales_mean_round > 44 & store$Sales_mean_round <= 47 ,1,0)
store$Rank7 <- ifelse(store$Sales_mean_round > 47 & store$Sales_mean_round <= 49 ,1,0) 
store$Rank7A <- ifelse(store$Sales_mean_round > 49 & store$Sales_mean_round <= 51 ,1,0)     
store$Rank8 <- ifelse(store$Sales_mean_round > 51 & store$Sales_mean_round <= 54 ,1,0) 
store$Rank8A <- ifelse(store$Sales_mean_round > 54 & store$Sales_mean_round <= 56 ,1,0)     
store$Rank9 <- ifelse(store$Sales_mean_round > 56 & store$Sales_mean_round <= 59 ,1,0)
store$Rank10 <- ifelse(store$Sales_mean_round > 59 & store$Sales_mean_round <= 61 ,1,0)
store$Rank11 <- ifelse(store$Sales_mean_round > 61  & store$Sales_mean_round <=64 ,1,0)
store$Rank12 <- ifelse(store$Sales_mean_round > 64  & store$Sales_mean_round <= 68 ,1,0)
store$Rank13 <- ifelse(store$Sales_mean_round > 68  & store$Sales_mean_round <= 72 ,1,0)
store$Rank14 <- ifelse(store$Sales_mean_round > 72  & store$Sales_mean_round <= 75 ,1,0) 
store$Rank14A <- ifelse(store$Sales_mean_round > 75  & store$Sales_mean_round <= 80 ,1,0)     
store$Rank15 <- ifelse(store$Sales_mean_round > 80  & store$Sales_mean_round <= 90 ,1,0)
store$Rank16 <- ifelse(store$Sales_mean_round > 90  & store$Sales_mean_round <= 100 ,1,0)
store$Rank17 <- ifelse(store$Sales_mean_round > 100  & store$Sales_mean_round <= 120 ,1,0)
store$Rank18 <- ifelse(store$Sales_mean_round > 120  & store$Sales_mean_round <= 300 ,1,0)


###creating Stores rating, according to the round(mean/100) of Sales
cat("aggregating Store by Customers and ranking in accordance \n")

agg3 <- aggregate(Customers ~ Store ,data=train,FUN=mean,na.rm=TRUE)
store$Customers_mean <- round(agg3$Customers)
store$CustomerRank1 <- ifelse(store$Customers_mean > 0 & store$Customers_mean <= 300,1,0)
store$CustomerRank2 <- ifelse(store$Customers_mean > 300 & store$Customers_mean <= 370,1,0)
store$CustomerRank2A <- ifelse(store$Customers_mean > 370 & store$Customers_mean <= 450,1,0)
store$CustomerRank2B <-  ifelse(store$Customers_mean > 450 & store$Customers_mean <= 500,1,0)
store$CustomerRank3 <- ifelse(store$Customers_mean > 500 & store$Customers_mean <= 570,1,0)
store$CustomerRank3A <- ifelse(store$Customers_mean > 570 & store$Customers_mean <= 630,1,0)
store$CustomerRank3B <- ifelse(store$Customers_mean > 630 & store$Customers_mean <= 700,1,0)
store$CustomerRank4 <- ifelse(store$Customers_mean > 700 & store$Customers_mean <= 780,1,0)
store$CustomerRank4B <- ifelse(store$Customers_mean > 780 & store$Customers_mean <= 900,1,0)
store$CustomerRank5 <- ifelse(store$Customers_mean > 900 & store$Customers_mean <= 1100,1,0)
store$CustomerRank6 <- ifelse(store$Customers_mean > 1100 & store$Customers_mean <= 1300,1,0)
store$CustomerRank7 <- ifelse(store$Customers_mean > 1300 & store$Customers_mean <= 1400,1,0)
store$CustomerRank8 <- ifelse(store$Customers_mean > 1400 & store$Customers_mean <= 1500,1,0)
store$CustomerRank9 <- ifelse(store$Customers_mean > 1500 & store$Customers_mean <= 2000,1,0)
store$CustomerRank10 <- ifelse(store$Customers_mean > 2000 & store$Customers_mean <= 5000,1,0)


store$Sales_mean_by_custs <- store$Sales_mean /  store$Customers_mean

###################################################################################################
## creating a model.matrix of dummy vars for store variable
store_df <- data.frame(store$Store)
names(store_df) <- c("store_num")
store_model_matrix <- model.matrix(~0+store_num,data=store_df)
store <- cbind(store,store_model_matrix)

store$State <- NULL

###################################################################################################

##Handle NAs on store data.frame
NAS <- data.frame(sapply(store,function(x) sum(is.na(x))))

names(NAS) <- c("Freq")
NAS$vars <- row.names(NAS)
row.names(NAS) <-NULL
NAS[NAS$Freq >0,2:1]

cat("creating dummy vars for PromoInterval \n")
store$PromoJan <- ifelse(store$PromoInterval == "Jan,Apr,Jul,Oct",1,0)
store$PromoFeb <- ifelse(store$PromoInterval == "Feb,May,Aug,Nov",1,0)
store$PromoMar <- ifelse(store$PromoInterval == "Mar,Jun,Sept,Dec",1,0)
store$PromoApr <- ifelse(store$PromoInterval == "Jan,Apr,Jul,Oct",1,0)
store$PromoMay <- ifelse(store$PromoInterval == "Feb,May,Aug,Nov",1,0)
store$PromoJun <- ifelse(store$PromoInterval == "Mar,Jun,Sept,Dec",1,0)
store$PromoJul <- ifelse(store$PromoInterval == "Jan,Apr,Jul,Oct",1,0)
store$PromoAug <- ifelse(store$PromoInterval == "Feb,May,Aug,Nov",1,0)
store$PromoSep <- ifelse(store$PromoInterval == "Mar,Jun,Sept,Dec",1,0)
store$PromoOct <- ifelse(store$PromoInterval == "Jan,Apr,Jul,Oct",1,0)
store$PromoNov <- ifelse(store$PromoInterval == "Feb,May,Aug,Nov",1,0)
store$PromoDec <- ifelse(store$PromoInterval == "Mar,Jun,Sept,Dec",1,0)
store$PromoInterval <- NULL
#######################################################################################
# stores_full <- store[!is.na(store$CompetitionDistance),]
# stores_nas <- store[is.na(store$CompetitionDistance),]
# stores_nas$Store
# 
# cat("filling NAs in CompetitionDistance \n")
# ##use rpart for filling NA values
# fit <- rpart(CompetitionDistance ~ StoreType + Assortment + CompetitionOpenSinceMonth + 
#                  CompetitionOpenSinceYear + Promo2 + Promo2SinceWeek + Promo2SinceYear + PromoJan + 
#                  PromoFeb + PromoMar + PromoApr + PromoMay + PromoJun + PromoJul + PromoAug +
#                  PromoSep + PromoOct + PromoNov + PromoDec + Sales_mean +Sales_mean_by_custs,
#              data=stores_full, method="anova")
# #method   =    "class" for a classification tree 
# #              "anova" for a regression tree
# 
# 
# fancyRpartPlot(fit)
# 
# Prediction <- predict(fit, stores_nas)
# Prediction <- round(Prediction)
# 
# 
# ##fill NAs with predicted values
# count <- 0
# # for(n in names(Prediction)){
# #     cat("prediction =", Prediction[n],"\n")
# #     count <- count + 1    
# #     store$CompetitionDistance[as.integer(n)] <- Prediction[count]
# # }

store$CompetitionDistance[is.na(store$CompetitionDistance)] <- 0

#store[is.na(store)]   <- 0

####################################################################################################
##implementing 2 different strategies for NA values
cat("handling NAs of Competition OpenSince Month/Year \n")
CompetitionOpenSinceMonth_most_freq <- (which.max(table(store$CompetitionOpenSinceMonth)))
CompetitionOpenSinceYear            <- (which.max(table(store$CompetitionOpenSinceYear)))
CompetitionOpenSinceMonth_most_freq
CompetitionOpenSinceYear
store$CompetitionOpenSinceMonth[is.na(store$CompetitionOpenSinceMonth)] <- CompetitionOpenSinceMonth_most_freq
store$CompetitionOpenSinceYear[is.na(store$CompetitionOpenSinceYear)] <- CompetitionOpenSinceYear
table(store$CompetitionOpenSinceYear)

store$Promo2SinceWeek[is.na(store$Promo2SinceWeek)] <- 50
store$Promo2SinceYear[is.na(store$Promo2SinceYear)] <- 2015
####################################################################################################
cat("Handling Promo2SinceDate \n")
#Handle + combine Promo2SinceYear + Promo2SinceWeek into Promo2SinceDate

Promo2SinceYear <-paste0(store$Promo2SinceYear,'-01-01')
Promo2SinceYear <- as.Date(Promo2SinceYear)

Promo2SinceWeek <- store$Promo2SinceWeek

Promo2SinceDate <- Promo2SinceYear + weeks(Promo2SinceWeek) - weeks(1)
store$Promo2SinceDate <- Promo2SinceDate
store$Promo2SinceYear <- NULL
store$Promo2SinceWeek <- NULL
####################################################################################################
cat("Handling CompetitionOpenSinceDate \n")
##Handling and combining CompetitionOpenSinceYear + CompetitionOpenSinceMonth into 
# CompetitionOpenSinceDate

CompetitionOpenSinceYear <- paste0(store$CompetitionOpenSinceYear,'-01-01')
CompetitionOpenSinceYear <- as.Date(CompetitionOpenSinceYear)
head(CompetitionOpenSinceYear)
CompetitionOpenSinceDate <- ymd(CompetitionOpenSinceYear + 
                                    months(store$CompetitionOpenSinceMonth) - months(1))

store$CompetitionOpenSinceDate <- as.Date(CompetitionOpenSinceDate)
store$CompetitionOpenSinceDate[is.na(store$CompetitionOpenSinceDate)] <- ymd("2015-10-01")

###
store$CompetitionOpenSinceDate  <- as.numeric(store$CompetitionOpenSinceDate)
store$CompetitionOpenSinceDate[store$CompetitionOpenSinceDate <0] <- 0
store$CompetitionSinceLevel0 <- ifelse(store$CompetitionOpenSinceDate == 0,1,0)
store$CompetitionSinceLevel1 <- ifelse(store$CompetitionOpenSinceDate > 0
                                      & store$CompetitionOpenSinceDate <= 12400 ,1,0)
store$CompetitionSinceLevel2 <- ifelse(store$CompetitionOpenSinceDate > 12400
                                      & store$CompetitionOpenSinceDate <= 13392 ,1,0)
store$CompetitionSinceLevel3 <- ifelse(store$CompetitionOpenSinceDate > 13392
                                      & store$CompetitionOpenSinceDate <= 14153 ,1,0)
store$CompetitionSinceLevel4 <- ifelse(store$CompetitionOpenSinceDate > 14153
                                      & store$CompetitionOpenSinceDate <= 14914 ,1,0)
store$CompetitionSinceLevel5 <- ifelse(store$CompetitionOpenSinceDate > 14914
                                      & store$CompetitionOpenSinceDate <= 15584 ,1,0)
store$CompetitionSinceLevel6 <- ifelse(store$CompetitionOpenSinceDate > 15584
                                      & store$CompetitionOpenSinceDate <= 16040 ,1,0)
store$CompetitionSinceLevel7 <- ifelse(store$CompetitionOpenSinceDate > 16040
                                      & store$CompetitionOpenSinceDate <= 16648 ,1,0)

#########
###!!!!!!
store$CompetitionSinceLevel <- 0
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate == 0,0,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 0
                                      & store$CompetitionOpenSinceDate <= 12400 ,1,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 12400
                                      & store$CompetitionOpenSinceDate <= 13392 ,2,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 13392
                                      & store$CompetitionOpenSinceDate <= 14153 ,3,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 14153
                                      & store$CompetitionOpenSinceDate <= 14914 ,4,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 14914
                                      & store$CompetitionOpenSinceDate <= 15584 ,5,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 15584
                                      & store$CompetitionOpenSinceDate <= 16040 ,6,store$CompetitionSinceLevel)
store$CompetitionSinceLevel <- ifelse(store$CompetitionOpenSinceDate > 16040
                                      & store$CompetitionOpenSinceDate <= 16648 ,7,store$CompetitionSinceLevel)
train <- merge(train,store[,c("Store","CompetitionSinceLevel")])

agg_Sales_by_CompSince <- aggregate(Sales ~ CompetitionSinceLevel + Store,data=train,FUN=mean,na.rm=TRUE)
names(agg_Sales_by_CompSince) <- c("CompetitionSinceLevel","Store","Sales_Mean_by_CompSince")
train <- merge(train,agg_Sales_by_CompSince[,c("Store","Sales_Mean_by_CompSince")],by="Store")
test <- merge(test,agg_Sales_by_CompSince[,c("Store","Sales_Mean_by_CompSince")],by="Store")
train$CompetitionSinceLevel <- NULL
##
train <- merge(train,store[,c("Store","Promo2")])
agg_Sales_by_Promo2 <- aggregate(Sales ~ Promo2 + Store,data=train,FUN=mean,na.rm=TRUE)
head(agg_Sales_by_Promo2)
names(agg_Sales_by_Promo2) <- c("Promo2Ind","Store","Sales_Mean_by_Promo2")
train <- merge(train,agg_Sales_by_Promo2[,c("Store","Sales_Mean_by_Promo2")],by="Store")
test <- merge(test,agg_Sales_by_Promo2[,c("Store","Sales_Mean_by_Promo2")],by="Store")
train$Promo2 <- NULL
#########

cat("Dividing CompetitionDistance into 4 dummy vars \n")
store$CompetitionDistance1 <- as.integer(ifelse(store$CompetitionDistance < 100,1,0))
store$CompetitionDistance2 <- as.integer(ifelse(store$CompetitionDistance >= 100 & 
                                         store$CompetitionDistance < 200,1,0))
store$CompetitionDistance3 <- as.integer(ifelse(store$CompetitionDistance >= 200 & 
                                         store$CompetitionDistance < 500 ,1,0))
store$CompetitionDistance4 <- as.integer(ifelse(store$CompetitionDistance >= 500 & 
                                         store$CompetitionDistance < 1000 ,1,0))
store$CompetitionDistance5 <- as.integer(ifelse(store$CompetitionDistance >= 1000 & 
                                         store$CompetitionDistance < 1500 ,1,0))
store$CompetitionDistance6 <- as.integer(ifelse(store$CompetitionDistance >= 1500 & 
                                         store$CompetitionDistance < 2000 ,1,0))
store$CompetitionDistance7 <- as.integer(ifelse(store$CompetitionDistance >= 2000 & 
                                         store$CompetitionDistance < 3000 ,1,0))
store$CompetitionDistance8 <- as.integer(ifelse(store$CompetitionDistance >= 3000 & 
                                         store$CompetitionDistance < 4500 ,1,0))
store$CompetitionDistance9 <- as.integer(ifelse(store$CompetitionDistance >= 4500 & 
                                         store$CompetitionDistance < 6500 ,1,0))
store$CompetitionDistance10 <- as.integer(ifelse(store$CompetitionDistance >= 6500,1,0))

store$CompetitionStrenght1 <- ifelse(store$CompetitionSinceLevel1 == 1 & 
                                         store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght2A <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght2B <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght2C <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght2D <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght2E <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght2F <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght2G <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght2H <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght2I <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght2J <- ifelse(store$CompetitionSinceLevel2 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)

#
#store$CompetitionStrengh3A <- ifelse(store$CompetitionSinceLevel3 == 1 & 
#                                          store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght3B <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght3C <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght3D <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght3E <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght3F <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght3G <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght3H <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght3I <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght3J <- ifelse(store$CompetitionSinceLevel3 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)
#
store$CompetitionStrengh4A <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght4B <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght4C <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght4D <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght4E <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght4F <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght4G <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght4H <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght4I <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght4J <- ifelse(store$CompetitionSinceLevel4 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)

#
store$CompetitionStrengh5A <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght5B <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght5C <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght5D <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght5E <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght5F <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght5G <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght5H <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght5I <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght5J <- ifelse(store$CompetitionSinceLevel5 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)
#
#store$CompetitionStrengh6A <- ifelse(store$CompetitionSinceLevel6 == 1 & 
#                                         store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght6B <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght6C <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght6D <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght6E <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght6F <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght6G <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght6H <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght6I <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght6J <- ifelse(store$CompetitionSinceLevel6 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)
#
#store$CompetitionStrengh7A <- ifelse(store$CompetitionSinceLevel7 == 1 & 
#                                         store$CompetitionDistance1 ==1 ,1,0)
store$CompetitionStrenght7B <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance2 ==1 ,1,0)
store$CompetitionStrenght7C <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance3 ==1 ,1,0)
store$CompetitionStrenght7D <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance4 ==1 ,1,0)
store$CompetitionStrenght7E <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance5 ==1 ,1,0)
store$CompetitionStrenght7F <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance6 ==1 ,1,0)
store$CompetitionStrenght7G <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance7 ==1 ,1,0)
store$CompetitionStrenght7H <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance8 ==1 ,1,0)
store$CompetitionStrenght7I <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance9 ==1 ,1,0)
store$CompetitionStrenght7J <- ifelse(store$CompetitionSinceLevel7 == 1 & 
                                          store$CompetitionDistance10 ==1 ,1,0)

#######
cat("Creating dummy vars for StoreType + Assortment \n")
##combine StoreType & Assortment
store$type_aa <- ifelse(store$StoreType == "a" & store$Assortment == "a",1,0)
store$type_ac <- ifelse(store$StoreType == "a" & store$Assortment == "c",1,0)
#
store$type_ba <- ifelse(store$StoreType == "b" & store$Assortment == "a",1,0)
store$type_bb <- ifelse(store$StoreType == "b" & store$Assortment == "b",1,0)
store$type_bc <- ifelse(store$StoreType == "b" & store$Assortment == "c",1,0)
#
store$type_ca <- ifelse(store$StoreType == "c" & store$Assortment == "a",1,0)
store$type_cc <- ifelse(store$StoreType == "c" & store$Assortment == "c",1,0)
#
store$type_da <- ifelse(store$StoreType == "d" & store$Assortment == "a",1,0)
store$type_dc <- ifelse(store$StoreType == "d" & store$Assortment == "c",1,0)

#######################################################################################
#train_t1 <- unique(train$Store)
#test_t1 <- unique(test$Store)
#train_filtered <- subset(train,Store %in% test_t1)
#train_filtered <- subset(train_filtered,Open==1)
#train <- train_filtered


test$Open[is.na(test$Open)] <- 0

train <- merge(train,store,by = "Store")
test <- merge(test,store,by = "Store")

# DayOfWeek:
#1 = Monday #2 = Tuesday #3 = Wednesday #4 = Thursday #5 = Friday
#6 = Saturday #7 = Sunday

##day(), wday(), yday(), week(), month(), year()

train$Date <- ymd(train$Date)
test$Date <- ymd(test$Date)


train$Year <- as.factor(lubridate::year(train$Date))
test$Year <-  as.factor(lubridate::year(test$Date))

train_df <- data.frame(train$Year)
names(train_df) <- c("Year")
train_model_matrix <- model.matrix(~0+Year,data=train_df)
train <- cbind(train,train_model_matrix)
test$Year2013 <- 0
test$Year2014 <- 0
test$Year2015 <- 1

train$Month <- as.factor(lubridate::month(train$Date))
test$Month <- as.factor(lubridate::month(test$Date))
matrix_df <- data.frame(train$Month)
names(matrix_df) <- c("Month_Num")
train_model_matrix <- model.matrix(~0+Month_Num,data=matrix_df)
train <- cbind(train,train_model_matrix)

matrix_df <- data.frame(test$Month)
names(matrix_df) <- c("Month_Num")
test_model_matrix <- model.matrix(~0+Month_Num,data=matrix_df)
test <- cbind(test,test_model_matrix)

test$Month_Num1 <- 0
test$Month_Num2 <- 0
test$Month_Num3 <- 0
test$Month_Num4 <- 0
test$Month_Num5 <- 0
test$Month_Num6 <- 0
test$Month_Num7 <- 0
test$Month_Num10 <- 0
test$Month_Num11 <- 0
test$Month_Num12 <- 0

###########################################################################################
train$DayOfMonth <- day(train$Date)
test$DayOfMonth <- day(test$Date)
## creating a model.matrix of dummy vars for store variable
train_DOM <- data.frame(train$DayOfMonth)
names(train_DOM) <- c("DOM")
model_matrix_train <- model.matrix(~0+DOM,data=train_DOM)
train <- cbind(train,model_matrix_train)


test_DOM <- data.frame(test$DayOfMonth)
names(test_DOM) <- c("DOM")
model_matrix_test <- model.matrix(~0+DOM,data=test_DOM)
test <- cbind(test,model_matrix_test)
###########################################################################################
train$yweek <- as.integer(week(train$Date))
test$yweek <- as.integer(week(test$Date))

train$DayOfYear <- yday(train$Date)
test$DayOfYear <- yday(test$Date)
#
cat("calculating difftime between Date and CompetitionOpenSinceDate \n")
# CompetitionExistSpan <- new_interval(train$CompetitionOpenSinceDate,train$Date)
# CompetitionExistSpan1 <- as.period(CompetitionExistSpan,units="day")
# CompetitionExistSpan_num <- day(CompetitionExistSpan1)
# train$CompetitionExistsDays <-CompetitionExistSpan_num
# 
# CompetitionExistSpan <- new_interval(test$CompetitionOpenSinceDate,test$Date)
# 
# CompetitionExistSpan1 <- as.period(CompetitionExistSpan,units="day")
# CompetitionExistSpan_num <- day(CompetitionExistSpan1)
# test$CompetitionExistsDays <-CompetitionExistSpan_num

train$CompetitionOpenSinceDate  <- as.numeric(train$CompetitionOpenSinceDate)
test$CompetitionOpenSinceDate <- as.numeric(test$CompetitionOpenSinceDate)

################################################################
cat("creating dummy var indicating if Promo2 is active in that month (since not all stores have Promo2) \n")
train$Promo2Active1 <- ifelse(train$Month == 1 &  train$PromoJan == 1,1,0)
train$Promo2Active2 <- ifelse(train$Month == 2 &  train$PromoFeb == 1,1,0)
train$Promo2Active3 <- ifelse(train$Month == 3 &  train$PromoMar == 1,1,0)
train$Promo2Active4 <- ifelse(train$Month == 4 &  train$PromoApr == 1,1,0)
train$Promo2Active5 <- ifelse(train$Month == 5 &  train$PromoMay == 1,1,0)
train$Promo2Active6 <- ifelse(train$Month == 6 &  train$PromoJun == 1,1,0)
train$Promo2Active7 <- ifelse(train$Month == 7 &  train$PromoJul == 1,1,0)
train$Promo2Active8 <- ifelse(train$Month == 8 &  train$PromoAug == 1,1,0)
train$Promo2Active9 <- ifelse(train$Month == 9 &  train$PromoSep == 1,1,0)
train$Promo2Active10 <- ifelse(train$Month == 10 &  train$PromoOct == 1,1,0)
train$Promo2Active11 <- ifelse(train$Month == 11 &  train$PromoNov == 1,1,0)
train$Promo2Active12 <- ifelse(train$Month == 12 &  train$PromoDec == 1,1,0)


test$Promo2Active1 <- ifelse(test$Month == 1 &  test$PromoJan == 1,1,0)
test$Promo2Active2 <- ifelse(test$Month == 2 &  test$PromoFeb == 1,1,0)
test$Promo2Active3 <- ifelse(test$Month == 3 &  test$PromoMar == 1,1,0)
test$Promo2Active4 <- ifelse(test$Month == 4 &  test$PromoApr == 1,1,0)
test$Promo2Active5 <- ifelse(test$Month == 5 &  test$PromoMay == 1,1,0)
test$Promo2Active6 <- ifelse(test$Month == 6 &  test$PromoJun == 1,1,0)
test$Promo2Active7 <- ifelse(test$Month == 7 &  test$PromoJul == 1,1,0)
test$Promo2Active8 <- ifelse(test$Month == 8 &  test$PromoAug == 1,1,0)
test$Promo2Active9 <- ifelse(test$Month == 9 &  test$PromoSep == 1,1,0)
test$Promo2Active10 <- ifelse(test$Month == 10 &  test$PromoOct == 1,1,0)
test$Promo2Active11 <- ifelse(test$Month == 11 &  test$PromoNov == 1,1,0)
test$Promo2Active12 <- ifelse(test$Month == 12 &  test$PromoDec == 1,1,0)

#########################################################################################################
train$Promo2SinceDate <- as.integer(train$Promo2SinceDate)
test$Promo2SinceDate <- as.integer(test$Promo2SinceDate)


############################################
train$StateHoliday1 <- ifelse(train$StateHoliday == 1,1,0)
train$StateHoliday2 <- ifelse(train$StateHoliday == 2,1,0)
train$StateHoliday3 <- ifelse(train$StateHoliday == 3,1,0)
train$StateHoliday4 <- ifelse(train$StateHoliday == 4,1,0)

test$StateHoliday1 <- ifelse(test$StateHoliday == 1,1,0)
test$StateHoliday2 <- ifelse(test$StateHoliday == 2,1,0)
test$StateHoliday3 <- ifelse(test$StateHoliday == 3,1,0)
test$StateHoliday4 <- ifelse(test$StateHoliday == 4,1,0)


train$OpenInSchoolHoliday <- ifelse(train$Open == 1 & train$SchoolHoliday ==1 ,1,0)
test$OpenInSchoolHoliday <- ifelse(test$Open == 1 & test$SchoolHoliday == 1 ,1,0)

train$OpenInPublicHoliday <- ifelse(train$Open == 1 & train$StateHoliday == "a" ,1,0)
test$OpenInPublicHoliday <- ifelse(test$Open == 1 & test$StateHoliday == "a" ,1,0)

###!!!


train$M1Promo2Week1 <- ifelse(train$Month ==1 & train$Promo2Active1==1 & 
                                           train$DayOfMonth < 7 ,1,0)
train$M1Promo2Week2 <- ifelse(train$Month ==1 & train$Promo2Active1==1 & 
                                train$DayOfMonth >  7 &train$DayOfMonth <= 13  ,1,0)
train$M1Promo2Week3 <- ifelse(train$Month ==1 & train$Promo2Active1==1 & 
                                  train$DayOfMonth >  14 &train$DayOfMonth <= 21  ,1,0)
train$M1Promo2Week4 <- ifelse(train$Month ==1 & train$Promo2Active1==1 & 
                                  train$DayOfMonth >  21 &train$DayOfMonth <= 28  ,1,0)
train$M1Promo2Week5 <- ifelse(train$Month ==1 & train$Promo2Active1==1 & 
                                  train$DayOfMonth >  28   ,1,0)


train$M8Promo2Week1 <- ifelse(train$Month ==8 & train$Promo2Active8==1 & 
                                  train$DayOfMonth < 7 ,1,0)
train$M8Promo2Week2 <- ifelse(train$Month ==8 & train$Promo2Active8==1 & 
                                  train$DayOfMonth >  7 &train$DayOfMonth <= 13  ,1,0)
train$M8Promo2Week3 <- ifelse(train$Month ==8 & train$Promo2Active8==1 & 
                                  train$DayOfMonth >  14 &train$DayOfMonth <= 21  ,1,0)
train$M8Promo2Week4 <- ifelse(train$Month ==8 & train$Promo2Active8==1 & 
                                  train$DayOfMonth >  21 &train$DayOfMonth <= 28  ,1,0)
train$M8Promo2Week5 <- ifelse(train$Month ==8 & train$Promo2Active8==1 & 
                                  train$DayOfMonth >  28   ,1,0)

train$M9Promo2Week1 <- ifelse(train$Month ==9 & train$Promo2Active9==1 & 
                                  train$DayOfMonth < 7 ,1,0)
train$M9Promo2Week2 <- ifelse(train$Month ==9 & train$Promo2Active9==1 & 
                                  train$DayOfMonth >  7 &train$DayOfMonth <= 13  ,1,0)
train$M9Promo2Week3 <- ifelse(train$Month ==9 & train$Promo2Active9==1 & 
                                  train$DayOfMonth >  14 &train$DayOfMonth <= 21  ,1,0)
train$M9Promo2Week4 <- ifelse(train$Month ==9 & train$Promo2Active9==1 & 
                                  train$DayOfMonth >  21 &train$DayOfMonth <= 28  ,1,0)
train$M9Promo2Week5 <- ifelse(train$Month ==9 & train$Promo2Active9==1 & 
                                  train$DayOfMonth >  28   ,1,0)


test$M1Promo2Week1 <- ifelse(test$Month ==1 & test$Promo2Active1==1 & 
                                 test$DayOfMonth < 7 ,1,0)
test$M1Promo2Week2 <- ifelse(test$Month ==1 & test$Promo2Active1==1 & 
                                 test$DayOfMonth >  7 &test$DayOfMonth <= 13  ,1,0)
test$M1Promo2Week3 <- ifelse(test$Month ==1 & test$Promo2Active1==1 & 
                                 test$DayOfMonth >  14 &test$DayOfMonth <= 21  ,1,0)
test$M1Promo2Week4 <- ifelse(test$Month ==1 & test$Promo2Active1==1 & 
                                 test$DayOfMonth >  21 &test$DayOfMonth <= 28  ,1,0)
test$M1Promo2Week5 <- ifelse(test$Month ==1 & test$Promo2Active1==1 & 
                                 test$DayOfMonth >  28   ,1,0)


test$M8Promo2Week1 <- ifelse(test$Month ==8 & test$Promo2Active8==1 & 
                                 test$DayOfMonth < 7 ,1,0)
test$M8Promo2Week2 <- ifelse(test$Month ==8 & test$Promo2Active8==1 & 
                                 test$DayOfMonth >  7 &test$DayOfMonth <= 13  ,1,0)
test$M8Promo2Week3 <- ifelse(test$Month ==8 & test$Promo2Active8==1 & 
                                 test$DayOfMonth >  14 &test$DayOfMonth <= 21  ,1,0)
test$M8Promo2Week4 <- ifelse(test$Month ==8 & test$Promo2Active8==1 & 
                                 test$DayOfMonth >  21 &test$DayOfMonth <= 28  ,1,0)
test$M8Promo2Week5 <- ifelse(test$Month ==8 & test$Promo2Active8==1 & 
                                 test$DayOfMonth >  28   ,1,0)

test$M9Promo2Week1 <- ifelse(test$Month ==9 & test$Promo2Active9==1 & 
                                 test$DayOfMonth < 7 ,1,0)
test$M9Promo2Week2 <- ifelse(test$Month ==9 & test$Promo2Active9==1 & 
                                 test$DayOfMonth >  7 &test$DayOfMonth <= 13  ,1,0)
test$M9Promo2Week3 <- ifelse(test$Month ==9 & test$Promo2Active9==1 & 
                                 test$DayOfMonth >  14 &test$DayOfMonth <= 21  ,1,0)
test$M9Promo2Week4 <- ifelse(test$Month ==9 & test$Promo2Active9==1 & 
                                 test$DayOfMonth >  21 &test$DayOfMonth <= 28  ,1,0)
test$M9Promo2Week5 <- ifelse(test$Month ==9 & test$Promo2Active9==1 & 
                                 test$DayOfMonth >  28   ,1,0)
##########################################################################################
train$Date <- NULL
test$Date <- NULL
train$Month <- NULL
test$Month <- NULL
train$DayOfMonth <- NULL
test$DayOfMonth <- NULL
train$Year <- NULL
test$Year <- NULL

train$Store <- NULL
test$Store <- NULL

train$CompetitionOpenSinceYear <- NULL
train$CompetitionOpenSinceMonth <- NULL
test$CompetitionOpenSinceYear <- NULL
test$CompetitionOpenSinceMonth <- NULL

train$StoreType <- NULL
test$StoreType <- NULL
train$Assortment <- NULL
test$Assortment <- NULL

train$StateHoliday <- NULL
test$StateHoliday <- NULL

train$X <- NULL
test$X <- NULL
############################################
names(train)
rem_vars <- which(names(train) %in% c("Customers"))
train <- train[,-rem_vars]
feature_names <- names(train)
feature_names

cat("making sure all levels appear both on train and test \n")
for (f in feature_names) {
#    cat("f =", f, class(train[,f][1]), "\n")
    if ( class(train[,f][1])  %in% c("character","factor")) {
        levels <- unique(c(train[,f], test[,f]))
        levels(train[,f]) <- levels
        levels(test[,f])  <- levels
    }
}

train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!='0'),]


cat("diff train\test: ", setdiff(names(train),names(test)) , "\n")
cat ("diff test\train : ", setdiff(names(test),names(train)), "\n")
rm(model_matrix_test,model_matrix_train,train_model_matrix,store,train_DOM,train_df,test_model_matrix)
rm(store_model_matrix)

###########
feature_names <- colnames(train)[!colnames(train) %in% c("Date","Sales","logSales","Customers")]
#feature_names <- names(train)[c(1,2,5:ncol(train))]

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature_names) {
    if (class(train[,f]) %in% c("factor","character")) {
        levels <- length(unique(c(train[,f], test[,f])))
        cat("f = " , f , " number of levels = ", levels, "\n")
    }
}


rm(list=ls())
gc()
setwd("/Users/aviblinder/Documents/Avi/MOOC/Data Science/Kaggle/Rossman Store Sales/")
Sys.setlocale("LC_ALL", "C")

library(ggplot2)

set.seed(1) 

train <- read.csv("",header = T,stringsAsFactor=F)
test <- read.csv("./Data/test.csv",header= T,stringsAsFactor=F)
store <- read.csv("./Data/store.csv",header=T,stringsAsFactor=F)

test <- fread("./Data/test.csv")
train <- fread("./Data/train.csv")
store <- fread("./Data/store.csv")

dim(train)
dim(test)
dim(stores)
head(train)
table(train$Store)

s1115 <- subset(train,Store==1115)
head(s1115)

library(data.table)
library(zoo)
library(forecast)
train[, Date := as.Date(Date)]
train[, lapply(.SD, function(x) length(unique(x)))]
prop.table(table(train$SchoolHoliday))
prop.table(table(test$SchoolHoliday))

table(stores$StoreType)
round(prop.table(table(stores$Assortment)),2)
summary(stores$CompetitionDistance)
table(stores$CompetitionOpenSinceMonth)
table(stores$CompetitionOpenSinceYear)

#merge CompetitionOpenSinceYear and CompetitionOpenSinceMonth
stores$CompetitionDistanceDate <- ifelse(stores$CompetitionOpenSinceMonth < 10,
                                         paste0(stores$CompetitionOpenSinceYear,'0',stores$CompetitionOpenSinceMonth),
                                         paste0(stores$CompetitionOpenSinceYear,stores$CompetitionOpenSinceMonth))

prop.table(table(train$Promo))
prop.table(table(test$Promo))

summary(stores$Promo2)
table(stores$Promo2SinceWeek)
table(stores$Promo2SinceYear)
table(stores$PromoInterval)

str(train_orig)
str(test_orig)



sum(is.na(train_orig))


sum(is.na(test_orig))
test_orig[!complete.cases(test_orig),]

sum(is.na(stores))
head(stores)

NASinColumns <- sapply(stores,function(x) sum(is.na(x)))
str(stores)
table(stores$CompetitionOpenSinceMonth)
table(stores$CompetitionOpenSinceYear)

train <- train_orig
test <- test_orig

train$SPC <- train$Sales/train$Customers
head(train$SPC)


table(test_orig$Date)


###### Plot influence of promo on sales
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Sales)) + 
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

##Plot influence of promo on # of customers
ggplot(train[train$Sales != 0 & train$Customers != 0],
       aes(x = factor(Promo), y = Customers)) + 
    geom_jitter(alpha = 0.1) +
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

with(train[train$Sales != 0 & train$Promo == 0], mean(Sales / Customers))
with(train[train$Sales != 0 & train$Promo == 1], mean(Sales / Customers))

prop.table(table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
                 ifelse(train$Promo, "Promo", "No promo")))

table(ifelse(train$Open == 1, "Opened", "Closed"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))

ggplot(train[Store == 85], 
       aes(x = Date, y = Sales, 
           color = factor(DayOfWeek == 7), shape = factor(DayOfWeek == 7))) + 
    geom_point(size = 3) + ggtitle("Sales of store 85 (True if sunday)")

ggplot(train[Sales != 0],
       aes(x = factor(DayOfWeek), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)


# There is a connection between store type and type of assortment
table(data.frame(Assortment = store$Assortment, StoreType = store$StoreType))

hist(store$CompetitionDistance, 100)
# Convert the CompetitionOpenSince... variables to one Date variable
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))
class(store$CompetitionOpenSince)
# One competitor opened 1900
hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, 
     main = "Years since opening of nearest competition")


#The different store types and assortment types imply different overall levels of sales and seem to be exhibiting different trends:
train_store <- merge(train, store, by = "Store")

ggplot(train_store[Sales != 0], 
       aes(x = as.Date(Date), y = Sales, color = factor(StoreType))) + 
    geom_smooth(size = 2)


#The effect of the distance to the next competitor is a little counterintuitive. Lower distance to the next competitor implies (slightly, possibly not significantly) higher sales. This may occur (my assumption) because stores with a low distance to the next competitor are located in inner cities or crowded regions with higher sales in general. Maybe the effects of being in a good / bad region and having a competitor / not having a competitor cancel out:

salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
                         by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
ggplot(salesByDist, aes(x = log(CompetitionDistance), y = log(MeanSales))) + 
    geom_point() + geom_smooth()

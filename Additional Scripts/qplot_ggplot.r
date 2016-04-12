train <- read.csv("./Data/train.csv")
store <- read.csv("./Data/store.csv")
train <- merge(train,store,by = "Store")
train$mean_store <- train$Sales/train$Customers
train$mean_store_round <- round(train$mean_store,0)
hist(train$mean_store_round,breaks=100)
tb1 <- data.frame(table(train$mean_store_round))
names(tb1) <- c("mean_sales_per_customer","freq")
tb1[order(-tb1$freq),]

agg1 <- aggregate(Sales ~ .,data=train[,c("Sales","month","year")],
                  FUN=function(x) c(mn =mean(x), n=length(x) ))
agg1

agg2 <- aggregate(Sales ~ .,data=train[,c("Sales","month","Store")],
                  FUN=function(x) c(mn =mean(x), n=length(x) ))
agg2 <- aggregate(Sales ~ .,data=train[,c("Sales","month","Store")],
                  FUN=mean)
head(agg2)

library(ggplot2)
#qplot(x, y, data=, color=, shape=, size=, alpha=, geom=, method=, formula=, 
#       facets=, xlim=, ylim= xlab=, ylab=, main=, sub=)

qplot(x=month,y=Sales,data=agg2[1:24,],color=Store,geom="point")

agg_plot <- ggplot(agg2[1:72,], aes(x =factor(month), y = Sales, color = as.factor(Store)))  +  geom_point(size = 5)
agg_plot + xlab("Month") + ylab("Sales Mean") + ggtitle ("Mean Sales by Store")
agg_plot +  scale_color_discrete(name = "Stores")
agg_plot + facet_wrap(~Store) 
agg_plot + geom_smooth()
store[store$Store %in% c(1:6),c("StoreType","Assortment")]

names(train)
g <- ggplot(train,aes(x=as.factor(Assortment),y=mean_store,color=as.factor(StoreType))) + geom_boxplot()
g
aov(train$mean_store ~ train$Assortment)


ggplot(train[train$Sales >0 & train$Customers > 0,],aes(x=log(Customers),y=log(Sales))) +
    geom_point(alpha = 0.2) + geom_smooth()


ggplot(train[train$Store == 85,], 
      aes(x = Date, y = Sales, 
          color = factor(DayOfWeek == 7))) + 
    geom_point(size = 3) + ggtitle("Sales of store 85 (True if sunday)")
#, shape = factor(DayOfWeek == 7)

#The variability of sales on sundays is quite high while the median is not:
ggplot(train[train$Sales != 0,],
       aes(x = factor(DayOfWeek), y = Sales)) + 
    geom_jitter(alpha = 0.1) + 
    geom_boxplot(color = "yellow", outlier.colour = NA, fill = NA)

###############################################################################
#Seasonlal plot:
library(lubridate)    
library(forecast)
temp <- as.data.frame(train)
temp$Date <- ymd(temp$Date)
temp$year <- as.factor(lubridate::year(temp$Date))
temp$month <- as.factor(lubridate::month(temp$Date))
agg <- aggregate(Sales ~ ., data=temp[, c("Sales", "month" ,"year")], function(x) sum(x)/1000)
agg1 <- aggregate(Sales ~ ., data=temp[, c("Sales", "month" ,"year")], FUN=mean)
head(agg);head(agg1)
SalesTS <- ts(agg$Sales, start=2013, frequency=12)
SalesTS1 <- ts(agg1$Sales, start=2013, frequency=12)
col = rainbow(3)
par(mfrow=c(1,1))
par(mfrow=c(1,3))
seasonplot(SalesTS, col=col, year.labels.left = TRUE, pch=19, 
           las=1)
seasonplot(SalesTS1, col=col, year.labels.left = TRUE, pch=19, las=1)
monthplot(SalesTS1)

########################  EDA #################################################
t1 <- train[train$Sales != 0 & train$Promo == 0, ]
mean(t1$Sales/t1$Customers)
t2 <- train[train$Sales != 0 & train$Promo != 0, ]
mean(t2$Sales/t2$Customers)

round(prop.table(table(ifelse(train$Sales != 0, "Sales > 0", "Sales = 0"),
      ifelse(train$Promo, "Promo", "No promo")),margin=2),2)

table(ifelse(train$Open == 1, "Opened", "Closed"),
      ifelse(train$Sales > 0, "Sales > 0", "Sales = 0"))
zerosPerStore <- sort(tapply(train$Sales, list(train$Store), function(x) sum(x == 0)))
hist(zerosPerStore)

agg1 <- aggregate(Sales ~ Store,data=train,function(x) sum(x==0))
agg1[order(agg1$Sales),]

#ploting a sample store with ZERO sales for a period of time
plot(train[train$Store == 972, "Sales"], ylab = "Sales", xlab = "Days", main = "Store 972")

library(zoo)
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))
head(store$CompetitionOpenSince)
hist(as.yearmon("2015-10") - store$CompetitionOpenSince, 100, 
     main = "Years since opening of nearest competition")

##
# Convert the Promo2Since... variables to one Date variable
# Assume that the promo starts on the first day of the week
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                      store$Promo2SinceWeek, 1, sep = "-"),
                                format = "%Y-%U-%u")
hist(as.numeric(as.POSIXct("2015-10-01", format = "%Y-%m-%d") - store$Promo2Since), 
     100, main = "Days since start of promo2",xlab="Days since start of Promo2")

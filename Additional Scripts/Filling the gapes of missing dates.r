#train <- read.csv(unz("./Data/train.csv.zip", "train.csv"),header = T,stringsAsFactor=F)
update.packages("readr")
library(readr)
train <- read_csv("./Data/train.csv", col_types=list(
    Store = col_integer(),
    DayOfWeek= col_integer(),
    Date = col_character(),
    Sales = col_integer(),
    Customers = col_integer(),
    Open = col_integer(),
    Promo = col_integer(),
    StateHoliday = col_character(),
    SchoolHoliday = col_integer()))
train$Date <- as.Date(train$Date)
library(dplyr)
#summarize(NumStores=n()) = "count() as NumStores"
by_Date <- train %>% group_by(Date) %>% summarize(NumStores=n())
head(by_Date)
by_Date[which(by_Date == 1115)]$Date
names(by_Date)
library(ggplot2)
ggplot(by_Date, aes(Date,NumStores)) + geom_line()

by_Date_Gap <- by_Date[by_Date$Date %in% 
                           seq(as.Date("2014-06-30"),as.Date("2015-01-01"),by="day"),]
head(by_Date_Gap,3)

#Identifying the Stores with Missing Data
all_stores <- unique(train$Store)
stores_reporting <- train$Store[train$Date == as.Date("2014-7-1")]
missing_stores <- all_stores[!(all_stores %in% stores_reporting)]
missing_stores

#Picking the first store in the list, Store 13, and plotting the data over time, we can clearly see the gap in the time series.

store13 <- subset(train, Store==13)
ggplot(store13, aes(Date,Sales)) +
    geom_point() +
    geom_smooth() + 
    ggtitle("Revenue for Store 13 over time")

#Given our above list of stores with missing data, we want to confirm if this list is constant over
# the period of the gap. The simples strategy is to create a new list for each additional
# day in the period and check the set difference with our original list. 
# If the difference is always zero, then we have a constant list of stores with missing data.

for (date in seq(as.Date("2014-7-2"),as.Date("2014-12-31"),by="day")) {
    stores_reporting <- train$Store[train$Date == date]
    missing_on_date <- all_stores[!(all_stores %in% stores_reporting)]
    if (length(setdiff(missing_on_date,missing_stores)) > 0) {
        cat("Date:",date," Difference in missing stores",setdiff(missing_on_date,missing_stores))
    } 
}

#additional date of missing sales:
stores_reporting <- train$Store[train$Date == as.Date("2013-1-1")]
additional_missing_store <- all_stores[!(all_stores %in% stores_reporting)]
additional_missing_store

#So only one store is missing data and given that itis New Year’s day, we could create a single row
# for this store with zeros for Sales, Customers, etc. 
# Alternatively, we can use a majority vote to basically say that this store follows the same pattern 
# as majority of other stores on that day. 
# Since this approach of using a majority vote will be used 
# later to impute part of the missing data in the large 6 month gap we have found, we will use it here
# as well. Once we have the missing row, we bind it to the initial training set and continue with 
# this.

date <- as.Date("2013-1-1")
day_of_week <- unique(train$DayOfWeek[train$Date == date])
sales <- as.numeric(names(which.max(table(train$Sales[train$Date == date]))))
customers <- as.numeric(names(which.max(table(train$Customers[train$Date == date]))))
open <- as.numeric(names(which.max(table(train$Open[train$Date == date]))))
promo <- as.numeric(names(which.max(table(train$Promo[train$Date == date]))))
state_holiday <- names(which.max(table(train$StateHoliday[train$Date == date])))
school_holiday <- as.numeric(names(which.max(table(train$SchoolHoliday[train$Date == date]))))

missing_row <- data.frame(Store = additional_missing_store,
                          DayOfWeek = day_of_week,
                          Date = date,
                          Sales = sales,
                          Customers = customers,
                          Open = open,
                          Promo = promo,
                          StateHoliday = state_holiday,
                          SchoolHoliday = school_holiday)

head(missing_row)
train <- rbind(train,missing_row)


#Imputing Missing Values
#As has been observed elsewhere, due to the large variations in the sales data, we can generally get
# better results working with the log of the sales values and then converting back for the final 
# submission. 
# For this reason we will add an extra column with the log of the sales values and impute values for 
# stores in our missing data set.
train$logSales <- log(train$Sales+1)

#The first step will be to construct an empty data frame of the size we require to impute our missing 
# data. One item that we can fill in right away is the Date column. 
# We essentially need to repeat the dates for the days in the gap by the number of stores for which we
# are missing data.
gap <- seq(as.Date("2014-7-1"),as.Date("2014-12-31"),by="day")
length(missing_stores);length(gap)
head(gap)
head(missing_stores)
n_missing <- length(gap)*length(missing_stores)
n_missing
missing_df <- data.frame(Store = integer(n_missing),
                         DayOfWeek = integer(n_missing),
                         Date = rep(gap,length(missing_stores)),  ##!!!! this creates the # of rows!!
                         Sales = integer(n_missing),
                         Customers = integer(n_missing),
                         Open = integer(n_missing),
                         Promo = integer(n_missing),
                         StateHoliday = character(n_missing),
                         SchoolHoliday = integer(n_missing),
                         logSales = numeric(n_missing),
                         stringsAsFactors=FALSE)
head(missing_df)
### --> majority vote for imputing values:
#########################################
#We are mainily interested in imputing values for the Sales and Customer columns but there are also
# other indicators that need to be added such as whether the store is Open, whether a Promo is on, 
# state and school holidays. For each of these latter indicators, we will use a majority vote to 
# impute values for the missing data. In other words, if the majority of stores are open we will 
# assume that this store is open, if the majority of stores are having a promo we will assume this 
# store is having a promo, etc.

for (date in gap) {
    missing_df$Store[missing_df$Date == date] <- missing_stores
    
    day_of_week <- unique(train$DayOfWeek[train$Date == date])
    missing_df$DayOfWeek[missing_df$Date == date] <- rep(day_of_week, length(missing_stores))
    
    missing_df$Sales[missing_df$Date == date] <- rep(NA, length(missing_stores))
    
    missing_df$Customers[missing_df$Date == date] <- rep(NA, length(missing_stores))
    
    open <- as.numeric(names(which.max(table(train$Open[train$Date == date]))))
    missing_df$Open[missing_df$Date == date] <- rep(open, length(missing_stores))
    
    promo <- as.numeric(names(which.max(table(train$Promo[train$Date == date]))))
    missing_df$Promo[missing_df$Date == date] <- rep(promo, length(missing_stores))
    
    state_holiday <- names(which.max(table(train$StateHoliday[train$Date == date])))
    missing_df$StateHoliday[missing_df$Date == date] <- rep(state_holiday, length(missing_stores))
    
    school_holiday <- as.numeric(names(which.max(table(train$SchoolHoliday[train$Date == date]))))
    missing_df$SchoolHoliday[missing_df$Date == date] <- rep(school_holiday, length(missing_stores))
    
    missing_df$logSales[missing_df$Date == date] <- rep(NA, length(missing_stores))
    
}

head(missing_df)

#Now that we have our data frame for the missing store data, we will bind this to existing training set and re-order everything according to date.

train_filled_gap <- rbind(train,missing_df)
train_filled_gap <- train_filled_gap[order(train_filled_gap$Date),]


# Finally, using the existing Sales, Customer and logSales values we can impute values for the
# missing time period. To do this we will use the median of existing Sales and Customer data but
# contrained to the day of the week, whether a promo is running or not, and obviously whether the
# store is open or not.

train_filled_gap <- train_filled_gap %>% 
    group_by(Store, DayOfWeek, Open, Promo) %>%
    mutate(Sales = as.integer(ifelse(is.na(Sales), 
                                     ifelse(Open == 0, 
                                            0,
                                            median(Sales, na.rm=T)), 
                                     Sales))) %>%
    mutate(Customers = as.integer(ifelse(is.na(Customers),
                                         ifelse(Open == 0, 
                                                0,
                                                median(Customers, na.rm=T)),
                                         Customers))) %>%
    mutate(logSales = ifelse(is.na(logSales),
                             ifelse(Open == 0,
                                    0,
                                    mean(logSales, na.rm=T)), 
                             logSales))

#To be thorough, we finally check if there are any remaining NA values in the Sales, Customers and logSales columns.

anything_missed <- subset(train_filled_gap, is.na(Sales) | is.na(Customers) | is.na(logSales))
anything_missed

write_csv(train_filled_gap,"train_filled_gap.csv")

#Forecasting

#The whole objective in filling the gap in the training data is to be able to use the functions in the forecast package to forecast sales for all stores over the entire time series. To test this, we will forecast sales for the example store we have been using.

#First, we will need to load the test dataset to get information we need on store openings and promos during the period we are trying to forecast.

test <- read_csv("./Data//test.csv", col_types=list(
    Id = col_integer(),
    Store = col_integer(),
    DayOfWeek= col_integer(),
#    Date = col_date(),
    Date = col_character(),
    Open = col_integer(),
    Promo = col_integer(),
    StateHoliday = col_character(),
    SchoolHoliday = col_integer()))
test$Date <- as.Date(test$Date)

store13_test <- subset(test, Store == 13)

#For our example, we will use the Fourier ARIMA method of forecasting daily data outlined on
# Rob Hyndman’s blog. To use this method, it requires dummy variables to indicate holidays. 
# For that we simply assume that if the store is closed, it is a holiday. 
# The Promo variable also shows regular periodic behaviour with promotions recurring regularly and
# running for a week at a time. 
# Thus we will include a regularisation variable for the Promo as well. 
# Putting these together yields the following plot for the raw forecast of sales over the test period.

holiday <- 1 - store13$Open
holidayf <- 1 - store13_test$Open
promo <- store13$Promo
promof <- store13_test$Promo
test_period <- max(store13_test$Date) - min(store13_test$Date) + 1
test_period
library(forecast)
y <- ts(store13$Sales, frequency=7)
z  <- fourier (ts(store13$Sales, frequency=365.25), K=5)
zf <- fourierf(ts(store13$Sales, frequency=365.25), K=5, h=test_period)
fit <- auto.arima(y, xreg=cbind(z,holiday,promo), seasonal=FALSE)
fc <-  forecast(fit, xreg=cbind(zf,holidayf,promof), h=test_period)
plot(fc)
head(fc)
#The raw forecast shows negative revenue on days that the store should be closed but this can be
# easily corrected by multiplying with the vector of open days.
head(test,10)
store13_test$Sales <- fc$mean*store13_test$Open
store13_test$Sales

# We can use the same forecasting method with logSales and then use the exponential to convert the 
# values back.
store13$logSales <- log(store13$Sales+1)

y <- ts(store13$logSales, frequency=7)
z <- fourier(ts(store13$logSales, frequency=365.25), K=5)
zf <- fourierf(ts(store13$logSales, frequency=365.25), K=5, h=test_period)
fit <- auto.arima(y, xreg=cbind(z,holiday,promo), seasonal=FALSE)
fc <- forecast(fit, xreg=cbind(zf,holidayf,promof), h=test_period)
plot(fc)

store13_test$logSales <- fc$mean*store13_test$Open
store13_test$logSales

#Plotting the two sets of forecasts shows small variations between the two that will need to be looked at in a separate study.

ggplot(store13_test, aes(Date)) + 
    geom_line(aes(y = store13_test$Sales, colour = "Sales")) + 
    geom_line(aes(y = exp(store13_test$logSales)-1, colour = "exp(logSales) - 1")) +
    xlab("Test Period") + 
    ylab("Forecast of Store 13 Sales")

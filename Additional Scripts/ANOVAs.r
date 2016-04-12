##perform ANOVA on dayOfWeek vs. Sales and vs. Customers
##perform ANOVA of PromoInterval vs. Sales and vs. Customers
##perform ANOVA on Assortment/StoreType vs Sales and vs. Customers

t1 <- subset(train,Sales !=0)
cor(t1$Store,t1$Customers,method = "pearson")
#plot(log(t1$Customers),log(t1$Sales))


salesByCustomer <- train$Sales/train$Customers
salesByCustomer[is.na(salesByCustomer)] <- 0
hist(salesByCustomer)

head(train$Sales)
##ANOVAs
library(gplots)

# x <- factor(train_store$DayOfWeek)
# y <- train_store$Customers

x <- train$Month
y <- salesByCustomer 

levels(x) <- as.character(c(1:12))

plotmeans(y ~ factor(x),digits=2,ccol="red",
          mean.labels=T,main="Plot of diffs between Xs")

aggregate(salesByCustomer ~ Month,data=train,FUN=mean)

boxplot(y ~ x,mean="Title",xlab="x title",ylab="y title",col=rainbow(7))
##points(means,col="black",pch=18) ????

aov_model1 <- aov(y~x)
summary(aov_model1)
tuk1 <- TukeyHSD(aov_model1)
tuk1
plot(tuk1)

###############

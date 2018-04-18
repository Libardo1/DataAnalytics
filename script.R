library(readr) 
library(caret)
library(nnet)
library(dplyr)

### Read files
train <- read.csv("~/Clemson/Data Mining/final project dataset/train.csv")

str(train)


## store data
#unzip("store.csv.zip")

store <- read_csv("~/Clemson/Data Mining/final project dataset/store.csv")
View(store)
str(store)

## test data
##unzip("test.csv.zip")

test <- read_csv("~/Clemson/Data Mining/final project dataset/test.csv")
View(test)
str(test)
# Transform date column in the train set

train$Date <- as.POSIXlt("10/01/2015", format="%m/%d/%Y") - 
  as.POSIXlt(train$Date, format="%m/%d/%Y")

train$Date <- as.numeric(train$Date / 1000) #Scale for factors

head(train$Date)

# Transform date in the test set

test$Date <- as.POSIXlt("10/01/2015", format="%m/%d/%Y") - 
  as.POSIXlt(test$Date, format="%m/%d/%Y")

test$Date <- as.numeric(test$Date / 1000) #Scale for factors

head(test$Date)

# There are NAs in the integer columns of  Store, so zero them out
store[is.na(store)] <- 0

store1 <- store                     ## Save Store data

# Convert categorical data to factor
store$StoreType <- as.factor(store$StoreType)
contrasts(store$StoreType)

store$Assortment <- as.factor(store$Assortment)

store$PromoInterval <- as.factor(store$PromoInterval)

contrasts(store$Assortment)
contrasts(store$PromoInterval)

## Create a new var to replace competiotion month/year
# Tranform all dates to same format

CompetitionSinceDate <- as.Date(with(store, paste(
  CompetitionOpenSinceYear,
  CompetitionOpenSinceMonth,
  '01',
  sep="-")), "%Y-%m-%d")


store$CompetitionSinceDate <- CompetitionSinceDate


# Transform the CompetitionSinceDate in store
store$CompetitionSinceDate <- as.POSIXlt("10/01/2015", format="%m/%d/%Y") - 
  as.POSIXlt(store$CompetitionSinceDate, format="%m/%d/%Y")

store$CompetitionSinceDate <- as.numeric(store$CompetitionSinceDate / 1000) 

store <- select(store, -CompetitionOpenSinceMonth,
                -CompetitionOpenSinceYear)

## Transform Promo2SinceWeek and Promo2SinceYear 
store$Promo2SinceWeek  <- store$Promo2SinceWeek * 7/30
store$Promo2SinceWeek <-  round(store$Promo2SinceWeek)

Promo2SinceDate <- as.Date(with(store, paste(
  Promo2SinceYear,
  Promo2SinceWeek,
  '01',
  sep="-")), "%Y-%m-%d")

store$Promo2SinceDate <- Promo2SinceDate

store$Promo2SinceDate <- as.POSIXlt("10/01/2015", format="%m/%d/%Y") - 
  as.POSIXlt(store$Promo2SinceDate, format="%m/%d/%Y")

store$Promo2SinceDate <- as.numeric(store$Promo2SinceDate / 1000) #Scale for factors

store <- select(store, -Promo2SinceWeek,
                -Promo2SinceYear)

store[is.na(store)] <- 0

## Convert promo2 to factor with 2 lavels - 0 , 1
store$Promo2 <- as.factor(store$Promo2)

## Now merge train and test with store data by Store ID
train <- merge(train,store, by = "Store")
test <- merge(test,store, by = "Store")

## See if anything missing in the merged files

length(train[is.na(train)])

length(test[is.na(test)])

# There are NAs in num/int fields, so convert them to zero
train[is.na(train)] <- 0
test[is.na(test)]   <- 0

# StateHoliday = 0 in both train and test. So, remove this var
train <- select(train, -StateHoliday)

test <- select(test, -StateHoliday)

## Let's see if factors can be reduced/simplified 
train_bkup <- train
summarise(group_by(train, DayOfWeek), mean(Sales))

## Based on mean sales for each DayOfWeek, we can keep only 5 categories in DayOfWeek

train$DayOfWeek <- as.character(train$DayOfWeek)


train$DayOfWeek[train$DayOfWeek== 5] <- 3
train$DayOfWeek[train$DayOfWeek== 6] <- 4

train$DayOfWeek <- as.factor(train$DayOfWeek)
contrasts(train$DayOfWeek)

## Let's look at store type
summarise(group_by(train, StoreType), mean(Sales))

## Based on mean sales for each store type, keep only a and b factor

train$StoreType <- as.character(train$StoreType)
train$StoreType[train$StoreType=="c"] <- "a"
train$StoreType[train$StoreType=="d"] <- "a"

train$StoreType <- as.factor(train$StoreType)
contrasts(train$StoreType)

## Let's look at Assortment
summarise(group_by(train, Assortment), mean(Sales))

# Based on mean sales for each Assortment, keep only a and b factor

train$Assortment <- as.character(train$Assortment)
train$Assortment[train$Assortment=="c"] <- "a"

train$Assortment <- as.factor(train$Assortment)
contrasts(train$Assortment)

## Let's look atPromoInterval 

summarise(group_by(train, PromoInterval), mean(Sales))

train$PromoInterval <- as.character(train$PromoInterval)

train$PromoInterval[train$PromoInterval=="Feb,May,Aug,Nov"]  <- 1
train$PromoInterval[train$PromoInterval=="Jan,Apr,Jul,Oct"]  <- 2
train$PromoInterval[train$PromoInterval=="Mar,Jun,Sept,Dec"]  <- 3

train$PromoInterval[train$PromoInterval=="3"]  <- 1

train$PromoInterval <- as.factor(train$PromoInterval)
contrasts(train$PromoInterval)

## Let's look at Promo2
summarise(group_by(train, Promo2), mean(Sales))

## Promo2 and PromoInterval seem to give the same kind of mean sales, so let's drop promo2

train <- select(train, -Promo2)

# Remove Customer from train as it's not available in test data
str(train)
train <- select(train, -Customers)

## Convert other categorical vars to type-factor

train$Open <- as.factor(train$Open)
train$Promo <- as.factor(train$Promo)
train$SchoolHoliday <- as.factor(train$SchoolHoliday)
contrasts(train$Open)
contrasts(train$Promo)
contrasts(train$SchoolHoliday)
str (train)

## Test data conversion

test$DayOfWeek <- as.character(test$DayOfWeek)

test$DayOfWeek[test$DayOfWeek== 5] <- 3
test$DayOfWeek[test$DayOfWeek== 6] <- 4

test$DayOfWeek <- as.factor(test$DayOfWeek)
contrasts(test$DayOfWeek)

## Based on mean sales for each store type, keep only a and b factor

test$StoreType <- as.character(test$StoreType)
test$StoreType[test$StoreType=="c"] <- "a"
test$StoreType[test$StoreType=="d"] <- "a"

test$StoreType <- as.factor(test$StoreType)
contrasts(test$StoreType)

# Based on mean sales for each Assortment, keep only a and b factor

test$Assortment <- as.character(test$Assortment)
test$Assortment[test$Assortment=="c"] <- "a"

test$Assortment <- as.factor(test$Assortment)
contrasts(test$Assortment)

## PromoInterval
test$PromoInterval <- as.character(test$PromoInterval)

test$PromoInterval[test$PromoInterval=="Feb,May,Aug,Nov"]  <- 1
test$PromoInterval[test$PromoInterval=="Jan,Apr,Jul,Oct"]  <- 2
test$PromoInterval[test$PromoInterval=="Mar,Jun,Sept,Dec"]  <- 3

test$PromoInterval[test$PromoInterval=="3"]  <- 1

test$PromoInterval <- as.factor(test$PromoInterval)
contrasts(test$PromoInterval)

## Convert other test categorical vars to type-factor

test$Open <- as.factor(test$Open)
test$Promo <- as.factor(test$Promo)
test$SchoolHoliday <- as.factor(test$SchoolHoliday)
contrasts(test$Open)
contrasts(test$Promo)
contrasts(test$SchoolHoliday)

## Remove Promo2 as the same has been removed from train as well
test <- select(test, -Promo2)

## Plots..

hist(train$Sales)

## The Histogram of Sales is skewed to the right 

hist(log(train$Sales))

train$Sales <- log(train$Sales + 1)
#summary(train$Sales)
#train$Sales[is.infinite(train$Sales)] <- 0

## Let's see CompetitionDistance - if normal in train

hist(log(train$CompetitionDistance))

train$CompetitionDistance <- log(train$CompetitionDistance + 1)

#train$CompetitionDistance[is.infinite(train$CompetitionDistance)] <- 0

## Let's see CompetitionDistance - if normal in test

hist(log(test$CompetitionDistance))

test$CompetitionDistance <- log(test$CompetitionDistance + 1)

test$CompetitionDistance[is.infinite(test$CompetitionDistance)] <- 0
str(test)


mygrid <- expand.grid(.decay=c(0.5, 0.1), .size=c(4,5,6))

trains <- train[sample(1:nrow(train), 100000, replace=TRUE),]

Sales.max <- max(trains$Sales)

summary(trains$Sales)

## From significance test, I have got only 3 vars as significant - Open,
## DayOfWeek and Promo. Let's train neural net by using the 3 independent vars only

 nnetfit <- train(Sales/Sales.max ~ Open + DayOfWeek + Promo, 
                 data=train,
                 method="nnet",
                 maxit=1000,
               # sampsize = 10000,
                 tuneGrid=mygrid,
                 trace=F) 
print(nnetfit)

##plot(nnetfit, rep = "best")

## From the above commented code for training NNET, I have got lowest RMSE with Size = 6 and decay = .1

Sales.max <- max(train$Sales)

summary(train$Sales/Sales.max)

nnet.fit <- nnet(Sales/Sales.max ~ Open + DayOfWeek + Promo, 
                 data=train, 
                 size=6, 
                 decay = 0.1,
               #  sampsize = 100000,
                 trace = T) 

nnet.yhat <- exp(predict(nnet.fit, test) * Sales.max)

yhat_round <- round(nnet.yhat)

summary(yhat_round)

yhat_round <- yhat_round - 1

submit1 <- cbind(test[,2], yhat_round)

submit1 <- as.data.frame(submit1)

colnames(submit1) <- c("Id", "Sales")

submit1 <- submit[order(submit1$Id, submit1$Sales), ]
head(submit1)

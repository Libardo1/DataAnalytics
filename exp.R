library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(randomForest)


cat("reading the train and test data\n")
train <- read.csv("~/Clemson/Data Mining/final project dataset/train.csv")
test  <- read_csv("~/Clemson/Data Mining/final project dataset/test.csv")
store <- read_csv("~/Clemson/Data Mining/final project dataset/store.csv")

#creating customer level variable
customer<-summarise(group_by(train, Store), m=mean(Customers),max=max(Customers),stdev=sd(Customers))

train<-left_join(train,store,by="Store")
test<-left_join(test,store,by="Store")

train<-left_join(train,customer,by="Store")
test<-left_join(test,customer,by="Store")

#creating factor variable
train$DayOfWeek<-as.factor(train$DayOfWeek)
train$Open<-as.factor(train$Open)
train$Promo<-as.factor(train$Promo)
train$StateHoliday<-as.factor(train$StateHoliday)
train$SchoolHoliday<-as.factor(train$SchoolHoliday)
train$StoreType<-as.factor(train$StoreType)
train$Assortment<-as.factor(train$Assortment)
train$Promo2<-as.factor(train$Promo2)

test$DayOfWeek<-as.factor(test$DayOfWeek)
test$Open<-as.factor(test$Open)
test$Promo<-as.factor(test$Promo)
test$StateHoliday<-as.factor(test$StateHoliday)
test$SchoolHoliday<-as.factor(test$SchoolHoliday)
test$StoreType<-as.factor(test$StoreType)
test$Assortment<-as.factor(test$Assortment)
test$Promo2<-as.factor(test$Promo2)

#feature engineering
train$Date<-as.Date(train$Date)
test$Date<-as.Date(test$Date)

#creating 3 variable month,quarter and year
train$month <- as.integer(format(train$Date, "%m"))
train$year <- as.integer(format(train$Date, "%y"))
train$day <- as.integer(format(train$Date, "%d"))
test$month <- as.integer(format(test$Date, "%m"))
test$year <- as.integer(format(test$Date, "%y"))
test$day <- as.integer(format(test$Date, "%d"))

train$month<-as.factor(train$month)
train$day<-as.factor(train$day)
train$year<-as.factor(train$year)

test$month<-as.factor(test$month)
test$day<-as.factor(test$day)
test$year<-as.factor(test$year)

#promo level features
train$PromoInterval<-as.factor(train$PromoInterval)

p1<-as.factor(ifelse(train$PromoInterval=="Feb,May,Aug,Nov",1,0))
p2<-as.factor(ifelse(train$PromoInterval=="Jan,Apr,Jul,Oct",1,0))
p3<-as.factor(ifelse(train$PromoInterval=="Mar,Jun,Sept,Dec",1,0))
p4<-as.factor(ifelse(train$PromoInterval=="",1,0))
promo<-data.frame(p1,p2,p3,p4)
train<-data.frame(train,promo)

p1<-as.factor(ifelse(test$PromoInterval=="Feb,May,Aug,Nov",1,0))
p2<-as.factor(ifelse(test$PromoInterval=="Jan,Apr,Jul,Oct",1,0))
p3<-as.factor(ifelse(test$PromoInterval=="Mar,Jun,Sept,Dec",1,0))
p4<-as.factor(ifelse(test$PromoInterval=="",1,0))
promo<-data.frame(p1,p2,p3,p4)
test<-data.frame(test,promo)

#creating plot to judge the impact of each varibale
ggplot(train,aes(month,Sales,fill=month))+geom_point()+geom_boxplot()
ggplot(train,aes(day,Sales,fill=day))+geom_point()+geom_boxplot()
ggplot(train,aes(year,Sales,fill=year))+geom_point()+geom_boxplot()
ggplot(train,aes(StoreType,Sales))+geom_point()+geom_boxplot(aes(fill = factor(Promo)))
ggplot(train,aes(StoreType,Sales))+geom_point()+geom_boxplot(aes(fill = factor(Assortment)))
ggplot(train,aes(month,Sales))+geom_point()+geom_boxplot(aes(fill = factor(Promo)))
ggplot(train,aes(month,Sales))+geom_point()+geom_boxplot(aes(fill = factor(Promo2)))
ggplot(train,aes(Promo,Sales))+geom_point()+geom_boxplot(aes(fill = factor(Promo2)))

#data summary
summary(train)

#exploratory plots
ggplot(train,aes(DayOfWeek,Sales))+geom_point()+geom_boxplot()
ggplot(train,aes(Open,Sales))+geom_point()+geom_boxplot()
ggplot(train,aes(Open,Sales))+geom_point()+geom_boxplot()
ggplot(train,aes(StateHoliday,Sales))+geom_point()+geom_boxplot()
ggplot(train,aes(SchoolHoliday,Sales))+geom_point()+geom_boxplot()


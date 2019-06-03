
# CAB FARE Project



rm(list = ls()) #remove all the stored data
setwd("C:\\Users\\I B BHATT\\Desktop\\machine learning\\edwisor-projects") #set working directory
getwd()
#Read the Data
cab_fare = read.csv('train_cab.csv',sep = ',')

#Dimension
dim(cab_fare)

head(cab_fare)

# Summary
summary(cab_fare)

#Datatypes
str(cab_fare)
cab_fare$fare_amount <- as.numeric(as.character(cab_fare$fare_amount))
#feature extraction
#1 from datetime
#install.packages('lubridate')
library(lubridate)
#converting into datetime
cab_fare$pickup_datetime<-ymd_hms(cab_fare$pickup_datetime)
#feature engineering
cab_fare$day_of_week = wday(cab_fare$pickup_datetime)
cab_fare$month = month(cab_fare$pickup_datetime)
cab_fare$year = year(cab_fare$pickup_datetime)
cab_fare$hour = hour(cab_fare$pickup_datetime)
#install.packages('chron')
library(chron)
cab_fare$weekend = chron::is.weekend(cab_fare$pickup_datetime)
cab_fare$weekend = factor(x = cab_fare$weekend, levels = c(FALSE,TRUE), labels = c(0,1))
table(cab_fare$weekend)
apply(cab_fare,2,function(x) sum(is.na(x)))#null value analysis
#omit
cab_fare <- na.omit(cab_fare)

table(cab_fare$passenger_count)
#dropping passenger with count more than 6
cab_fare <- cab_fare[!(cab_fare$passenger_count>6 | cab_fare$passenger_count==0 | cab_fare$passenger<1|cab_fare$passenger==1.3),]
cab_fare<- cab_fare[!(cab_fare$fare_amount==0),]
#converting into categorical
for (i in seq(7, 11, by=1)){
  cab_fare[,i] = as.factor(cab_fare[,i])
}
str(cab_fare)
#outlier analysis
#install.packages('ggplot2')
#install.packages('dplyr')
library(ggplot2)
library(gplots)
library(dplyr)
library(gridExtra)
box_fare_amount = ggplot(cab_fare,aes(y = fare_amount,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for cab_fare ", y="fare")
box_pickup_longitude = ggplot(cab_fare,aes(y = pickup_longitude,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for pickup longitude", y="longitude")
box_pickup_latitude = ggplot(cab_fare,aes(y = pickup_latitude,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for pickup_latitude", y="latitude") 
box_dropoff_longitude = ggplot(cab_fare,aes(y = dropoff_longitude,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for dropoff_longitude", y="longitude")
box_dropoff_latitude = ggplot(cab_fare,aes(y = dropoff_latitude,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for dropoff_latitude", y="latitude")
gridExtra::grid.arrange(box_fare_amount,box_pickup_longitude,box_pickup_latitude,box_dropoff_longitude,box_dropoff_latitude,ncol=2)
#removal of outliers
cab_fare = cab_fare[which(!cab_fare$fare_amount %in% boxplot.stats(cab_fare$fare_amount)$out),]
cab_fare = cab_fare[which(!cab_fare$pickup_longitude %in% boxplot.stats(cab_fare$pickup_longitude)$out),]
cab_fare = cab_fare[which(!cab_fare$pickup_latitude %in% boxplot.stats(cab_fare$pickup_latitude)$out),]
cab_fare = cab_fare[which(!cab_fare$dropoff_longitude %in% boxplot.stats(cab_fare$dropoff_longitude)$out),]
cab_fare = cab_fare[which(!cab_fare$dropoff_latitude %in% boxplot.stats(cab_fare$dropoff_latitude)$out),]

dim(cab_fare)
#trip distance
#install.packages('geosphere')
library(geosphere)
cab_fare$trip_distance<- distHaversine(cab_fare[,3:4], cab_fare[,5:6])
#cab_fare$trip_distance<- distm(c(cab_fare$pickup_longitude, cab_fare$pickup_latitude), c(cab_fare$dropoff_longitude, cab_fare$dropoff_latitude), fun = distHaversine)
cab_fare$trip_distance <- cab_fare$trip_distance/1000.0 #to convert into kms
cab_fare$trip_distance

#histogram to check normal distribution between variables
hist_distance = ggplot(data=cab_fare, aes(cab_fare$trip_distance)) + geom_histogram(breaks=seq(0, 40, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for Trip_distance", x="trip_distance", y="Count")
hist_fareamount = ggplot(data=cab_fare, aes(cab_fare$fare_amount)) + geom_histogram(breaks=seq(0, 60, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for fare amount", x="fare_amount", y="Count")
gridExtra::grid.arrange(hist_distance,hist_fareamount,ncol=2)
#scatter plot of latitude and longitude with respect to the fare
scatter_pickup = ggplot(cab_fare,aes(pickup_latitude,pickup_longitude))+geom_point(aes(color = fare_amount))+labs(title = 'Heatmap of pickup',x = 'latitude',y='longitude')
scatter_drop_off = ggplot(cab_fare,aes(dropoff_latitude,dropoff_longitude))+geom_point(aes(color = (fare_amount)))+labs(title = 'Heatmap of dropoff',x = 'latitude',y='longitude')
gridExtra::grid.arrange(scatter_pickup,scatter_drop_off,ncol=2)
ggplot(cab_fare,aes(trip_distance,fare_amount))+geom_point(color='blue')+labs(title = 'Scatterplot of Trip Distance and amount',x = 'Trip Distance',y='fare_amount')

#bar graph of categorical variables
bar_passenger_count = ggplot(cab_fare, aes(passenger_count,fare_amount)) + geom_col() + labs(title="bargraph for passenger_count", x="passenger_count", y="fare_amount")
bar_year = ggplot(cab_fare, aes(year,fare_amount)) + geom_col() + labs(title="bargraph for Year", x="Year", y="fare_amount")
bar_month = ggplot(cab_fare, aes(month,fare_amount)) + geom_col() + labs(title="bargraph for month", x="month", y="fare_amount")
bar_hour = ggplot(cab_fare, aes(hour,fare_amount)) + geom_col() + labs(title="bargraph for hour", x="Hour", y="fare_amount")
bar_day_of_week = ggplot(cab_fare, aes(day_of_week,fare_amount)) + geom_col() + labs(title="bargraph for day", x="day", y="fare_amount")
bar_weekend = ggplot(cab_fare, aes(weekend,fare_amount)) + geom_col() + labs(title="bargraph for weekend", x="weekend", y="fare_amount")
gridExtra::grid.arrange(bar_passenger_count,bar_year,bar_month,bar_hour,bar_day_of_week,bar_weekend,ncol=2)

#correlation matrix
cor(cab_fare %>% select_if(is.numeric)) #pickup_latitude and longitude have multicolinearity 

#anova 
anova_passenger <- aov(fare_amount~passenger_count, data = cab_fare)
summary(anova_passenger)
anova_year <- aov(fare_amount~year, data = cab_fare)
summary(anova_year)
anova_month <- aov(fare_amount~month, data = cab_fare)
summary(anova_month)
anova_hour <- aov(fare_amount~hour, data = cab_fare)
summary(anova_hour)
anova_day <- aov(fare_amount~day_of_week, data = cab_fare)
summary(anova_day)
anova_weekend <- aov(fare_amount~weekend, data = cab_fare)
summary(anova_weekend)
#day_of_week and weekend are insignificant.

#removing variables
cab_fare =  subset(cab_fare, select = -c(pickup_datetime,pickup_longitude,day_of_week,weekend))
head(cab_fare)

#Modelling
library(caTools)
set.seed(0)
#split<- sample.split(cab_fare$fare_amount, SplitRatio = 0.8)
#train<-subset(cab_fare,split == TRUE)
#test<-subset(cab_fare,split== FALSE)
#train and test
train_index = sample(1:nrow(cab_fare),0.8*nrow(cab_fare))
train = cab_fare[train_index,]
test = cab_fare[-train_index,]
#linear regression
model_lm = lm(fare_amount~.,data= train)
summary(model_lm)

predict_lm = predict(model_lm,test[,-1])
Rsquare = (cor(test[,1],predict_lm))^2
Rsquare


#RMSE
#install.packages('DMwR')
library(DMwR)
regr.eval(test[,1],predict_lm,stats = {"rmse"})
#1.99
#dropping variables
train = subset(train, select = -c(pickup_latitude,dropoff_latitude))
train = subset(test, select = -c(pickup_latitude,dropoff_latitude))
#new model
model_lm = lm(fare_amount~.,data= train)
summary(model_lm)

predict_lm = predict(model_lm,test[,-1])
Rsquare = (cor(test[,1],predict_lm))^2
Rsquare
#0.7249
regr.eval(test[,1],predict_lm,stats = {"rmse"})
#1.99
#decision tree
library(rpart)
model_dtr = rpart(fare_amount~.,data= train, method = 'anova')
summary(model_dtr)
#predict
predict_dtr = predict(model_dtr,test[,-1])
#decision tree plot
plot(model_dtr)
text(model_dtr)
#RMSE
regr.eval(test[,1],predict_dtr,stats = {"rmse"})
#2.12
Rsquare = (cor(test[,1],predict_dtr))^2
Rsquare
#0.689
#random forest
library(randomForest)

model_rfr = randomForest(fare_amount~.,data=train, ntree = 200)
predict_rfr = predict(model_rfr,test[,-1])
regr.eval(test[,1],predict_rfr,stats = {"rmse"})
#0.966
Rsquare = (cor(test[,1],predict_rfr))^2
Rsquare
#0.9522
#test processing and predictions
test_fare <- read.csv('test.csv',sep = ',')
head(test_fare)
kapil <- test_fare$key
library(lubridate)
test_fare$pickup_datetime<-ymd_hms(test_fare$pickup_datetime)

test_fare$month = month(test_fare$pickup_datetime)
test_fare$year = year(test_fare$pickup_datetime)
test_fare$hour = hour(test_fare$pickup_datetime)
head(train)
str(test_fare)
test_fare$trip_distance<- distHaversine(test_fare[,3:4], test_fare[,5:6])

test_fare$trip_distance <- test_fare$trip_distance/1000.0 #to convert into kms
head(test_fare)

test_fare = subset(test_fare,select = -c(key,pickup_datetime,pickup_longitude,pickup_latitude,dropoff_latitude))
for (i in seq(2, 5, by=1)){
  test_fare[,i] = as.factor(test_fare[,i])
}
str(test_fare)
fare_amount = predict(model_rfr,test_fare)
answer<- data.frame(cbind(as.character(test_fare$key),predict_test))
head(answer)
names(answer)[1] <- "key"
names(answer)[2] <- "fare_amount"
write.csv(answer,'answer.csv',row.names=FALSE)


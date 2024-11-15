library(openxlsx)
library(mosaic)
data <-read.xlsx("dataset01.xlsx")

#Pre-processing
str(data)
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)

#Question 1: Analyze variable STARTING_PRICE
summary(data$STARTING_PRICE)

#histogram
histogram(~STARTING_PRICE, data=data, breaks=20)  #Right-skew
#Boxplot
boxplot(data$STARTING_PRICE) #A lot of possible outlier 


#Question 2: Analyze Region and Type
tally(~ REGION + TYPE, data = data, margins = TRUE, format="proportion")


#Question 3: Analyze Region and Area
histogram(~ AREA | REGION, data = data)
boxplot(AREA ~ REGION, data = data)


#Question 4: Analyze STARTING_PRICE and AREA
plot(data$AREA,data$STARTING_PRICE,xlab="Area",
     ylab="Starting Price", main="Grafte",pch=4)

#Question 5:Fit linear regression between STARTING_PRICE and AREA
simple_reg = lm(STARTING_PRICE~AREA, data=data)
summary(simple_reg)
abline(simple_reg, col = "#FF1F06")

#Question 6:  Fit linear regression between STARTING_PRICE and REGION,TYPE,BALCONY,ROOMS and AREA
data_num <- select(data,ROOMS,AREA,STARTING_PRICE)
plot(data_num)

multiple_reg = lm(STARTING_PRICE~REGION+TYPE+ROOMS+AREA+BALCONY, data=data)
summary(multiple_reg)
abline(multiple_reg, col = "#488606")

#Question 7: Predict housing starting price
test <-read.xlsx("test.xlsx")
str(test)
test$REGION<-factor(test$REGION)
test$TYPE<-factor(test$TYPE)
test$BALCONY<-factor(test$BALCONY)
View(data)

predict(multiple_reg,test)   #Predict using our model
# Our Original data
data[data$ID %in% c(629,718,1534,1695,1864,2138),]



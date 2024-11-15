##################
## Asigment 1
## Code by Steven Rubio

library(openxlsx)
library(mosaic)
library(RColorBrewer)
data <-read.xlsx("dataset01.xlsx")

### Pre-processing ----
str(data)
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)


### Question 1: Analyze variable STARTING_PRICE ----
summary(data$STARTING_PRICE)
#histogram
histogram(~STARTING_PRICE, data=data, breaks=20, main="Distribution of Starting price", 
          ylim=c(0,30), xlab = "Starting Price", ylab = "Count")  #Right-skew
#Boxplot
boxplot(data$STARTING_PRICE) #A lot of possible outlier 

### Question 2: Analyze Region and Type ----
dist1 = tally(~ TYPE + REGION, data = data, margins = FALSE, format="percent")
barplot(dist1,col=colors()[c(23,89,12)], beside=T,ylim=c(0,50))

dist2 = tally(~ REGION+TYPE, data = data, margins = FALSE, format="percent")
barplot(dist2,col=colors()[c(23,89,12,112,200)], beside=T,,ylim=c(0,50))

# Transform this data in %
data_percentage <- apply(dist1, 2, function(x){x*100/sum(x,na.rm=T)})

# Make a stacked barplot--> it will be in %!
barplot(data_percentage, col=coul , border="white", xlab="group")

### Question 3: Analyze Region and Area ----
histogram(~ AREA | REGION, data = data)
boxplot(AREA ~ REGION, data = data)


### Question 4: Analyze STARTING_PRICE and AREA ----
plot(data$AREA,data$STARTING_PRICE,xlab="Area",
     ylab="Starting Price", main="Grafte",pch=4)

### Question 5:Fit linear regression between STARTING_PRICE and AREA ----
simple_reg = lm(STARTING_PRICE~AREA, data=data)
summary(simple_reg)
abline(simple_reg, col = "#FF1F06")

### Question 6:  ----
# Fit linear regression between STARTING_PRICE and REGION,TYPE,BALCONY,ROOMS and AREA
data_num <- select(data,ROOMS,AREA,STARTING_PRICE)
plot(data_num)

multiple_reg = lm(STARTING_PRICE~REGION+TYPE+ROOMS+AREA+BALCONY, data=data)
summary(multiple_reg)
abline(multiple_reg, col = "#488606")

### Question 7: Predict housing starting price ----
test <-read.xlsx("test.xlsx")
str(test)
test$REGION<-factor(test$REGION)
test$TYPE<-factor(test$TYPE)
test$BALCONY<-factor(test$BALCONY)
View(data)

predict(multiple_reg,test)   #Predict using our model
# Our Original data
data[data$ID %in% c(629,718,1534,1695,1864,2138),]



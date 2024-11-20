##################
## Asigment 1
## Code by Steven Rubio

library(openxlsx)
library(mosaic)
library(RColorBrewer)
library(DescTools)
library(e1071)
library(scales)

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
skewness(data$STARTING_PRICE)

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
#barplot(data_percentage, col=coul , border="white", xlab="group")

### Question 3: Analyze Region and Area ----
histogram(~ AREA | REGION, data = data)
boxplot(AREA ~ REGION, data = data)

### Question 4: Analyze STARTING_PRICE and AREA ----
#We see that they are positively related
#cor(data$AREA, data$STARTING_PRICE)

print(paste0("Corelation between Starting price and Area: ",cor(data$AREA, data$STARTING_PRICE)))
plot(data$AREA,data$STARTING_PRICE,xlab="Area",
     ylab="Starting Price", main="Grafte",pch=4)

#What if we do it per Region:
for (i in levels(data$REGION)) {
  data_region <- data[data$REGION == i, ]
  print(paste0("Region: ",i,". Cor Value: ",round(cor(data_region$AREA, data_region$STARTING_PRICE),digits=4) )) 
  group_reg = lm(STARTING_PRICE~AREA, data=data_region)
  print(paste0("Coefficients ",round(group_reg$coefficients[1],2)," ,",round(group_reg$coefficients[2],2)))
  print("")
}
#What if we do it per TYPE:
for (i in levels(data$TYPE)) {
  data_type <- data[data$TYPE == i, ]
  print(paste0("TYPE: ",i,". Cor Value: ",cor(data_type$AREA, data_type$STARTING_PRICE))) 
  
}

### Question 5:Fit linear regression between STARTING_PRICE and AREA ----
simple_reg = lm(STARTING_PRICE~AREA, data=data)
summary(simple_reg)
abline(simple_reg, col = "#FF1F06")

#It seems like we get better results if we analyze the data based on  REGION


### Question 6:  ----
# Fit linear regression between STARTING_PRICE and REGION,TYPE,BALCONY,ROOMS and AREA
data_num <- select(data,ROOMS,AREA,STARTING_PRICE)
plot(data_num)
data$STARTING_PRICE <-data$STARTING_PRICE/ 1000000

multiple_reg = lm(STARTING_PRICE~REGION+TYPE+AREA+BALCONY+ROOMS, data=data)
summary(multiple_reg) #0.5498 - 0.5439

multiple_reg2 = lm(STARTING_PRICE~REGION+TYPE+AREA+BALCONY+ROOMS+REGION*AREA, data=data)
summary(multiple_reg2)

multiple_reg3 = lm(STARTING_PRICE~REGION+TYPE+AREA+BALCONY+ROOMS+ TYPE*AREA, data=data)
summary(multiple_reg3)

multiple_reg4 = lm(STARTING_PRICE~REGION+TYPE+AREA+BALCONY+ROOMS+REGION*AREA+TYPE*AREA, data=data)
summary(multiple_reg4)

multiple_reg5 = lm(STARTING_PRICE~REGION+TYPE+AREA, data=data)
summary(multiple_reg5)

multiple_reg6 = lm(STARTING_PRICE~REGION+TYPE+AREA+REGION*AREA+TYPE*AREA, data=data)
summary(multiple_reg6)
round(multiple_reg6$coefficients, 3)

multiple_reg7 = lm(STARTING_PRICE~REGION+AREA+log(AREA)+REGION*AREA, data=data)
summary(multiple_reg7)

### Question 7: Predict housing starting price ----
test <-read.xlsx("test.xlsx")
str(test)
test$REGION<-factor(test$REGION)
test$TYPE<-factor(test$TYPE)
test$BALCONY<-factor(test$BALCONY)
View(data)

p1<-1000000*predict(multiple_reg,test)   #Predict using first model
p2<-1000000*predict(multiple_reg6,test)   #Predict using last model model

percent((p1 - p2) / p1)

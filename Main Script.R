##################
## Asigment 1
## Statistics and Data Analysis for Computer and Systems Sciences -SDA-.
## Code by:
##          - Caroline Birkehammar
##          - Faezeh Karegar Hojatabadi 
##          - Pablo Paras Ochoa
##          - Steven Hiram Rubio Vasquez
###################

#Librarys
library(openxlsx)
library(mosaic)
library(RColorBrewer)
library(DescTools)
library(e1071)
library(scales)

#Data Pre-processing
data <-read.xlsx("dataset01.xlsx")
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)

test <-read.xlsx("test.xlsx")
str(test)
test$REGION<-factor(test$REGION)
test$TYPE<-factor(test$TYPE)
test$BALCONY<-factor(test$BALCONY)
### Question 1: ----

### Question 2: ----

### Question 3: ----

### Question 4: ----

### Question 5: ----

### Question 6: ----

### Question 7: ----
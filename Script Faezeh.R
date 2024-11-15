# Faezeh's Script :D
library(openxlsx)
library(mosaic)
library(ggplot2)

data <- read.xlsx("dataset01.xlsx")
summary(data$STARTING_PRICE)
data$STARTING_PRICE_M <- data$STARTING_PRICE / 1e6

#First Question
# Histogram and density plot
hist(data$STARTING_PRICE_M, 
     main = "Distribution of STARTING_PRICE (in millions SEK)", 
     xlab = "Starting Price (Million SEK)", 
     col = "blue", 
     breaks = 20)

boxplot(data$STARTING_PRICE_M, 
        main = "Boxplot of STARTING_PRICE (in millions SEK)", 
        ylab = "Starting Price (Million SEK)", 
        col = "cyan")

#Second Question
table(data$REGION, data$TYPE)


barplot(table(data$TYPE, data$REGION), 
        main = "Distribution of Housing Types by Region", 
        xlab = "Region", 
        ylab = "Count", 
        col = c("purple", "green", "pink"), 
        legend = TRUE, 
        beside = FALSE)

#Third Question
tapply(data$AREA, data$REGION, summary)  #summary of each subset of AREA
boxplot(AREA ~ REGION, data = data,
        main = "Distribution of Housing Unit Sizes (AREA) by Region",
        xlab = "Region", ylab = "Area (Square Meters)",
        col = "pink", border = "black")

#Forth Question
plot(data$AREA, data$STARTING_PRICE, 
     main = "Relationship between STARTING_PRICE and AREA", 
     xlab = "Area (Square Meters)", 
     ylab = "Starting Price (SEK)", 
     pch = 8, col = "blue", cex = 0.6)

#Fifth Question
model <- lm(STARTING_PRICE ~ AREA, data = data)
abline(model, col = "red", lwd = 3)
summary(model)
cor(data$AREA, data$STARTING_PRICE)

#Sixth Question

# Convert categorical variables to factors 
data$REGION <- as.factor(data$REGION)
data$TYPE <- as.factor(data$TYPE)
data$BALCONY <- as.factor(data$BALCONY)

# Fit the multiple linear regression model
multi_model <- lm(STARTING_PRICE ~ AREA + ROOMS + REGION + TYPE + BALCONY, data = data)

# Summary of the model
summary(multi_model)
# Bar plot of coefficients
coeff <- coef(multi_model)[-1] # Exclude the intercept
barplot(coeff, horiz = TRUE, las = 1, col = "lightblue",
        main = "Regression Coefficients",
        xlab = "Coefficient Value")
abline(v = 0, col = "red", lwd = 2)


#Seventh Question
# Load the test data set
test_data <- read.xlsx("test.xlsx")  # Replace with your file path

# Ensure categorical variables are factors (if needed)
test_data$REGION <- as.factor(test_data$REGION)
test_data$TYPE <- as.factor(test_data$TYPE)
test_data$BALCONY <- as.factor(test_data$BALCONY)

# Predict starting prices using the fitted model
test_data$PREDICTED_PRICE <- predict(multi_model, newdata = test_data)

# View the results
print(test_data)










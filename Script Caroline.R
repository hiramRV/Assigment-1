# This is Caroline's code

# Hola Patito Pablo and Big boss Steven and Fantastic Faezeh
# Also sorry 


library(readxl)
test <- read_excel("test.xlsx")
data <- read_excel("dataset01.xlsx")

# Data transformation

data$STARTING_PRICE <- data$STARTING_PRICE / 1000000
data$REGION <- as.factor(data$REGION)
data$TYPE <- as.factor(data$TYPE)
data$BALCONY <- as.factor(data$BALCONY)

# Q1 - Analyze STARTING_PRICE in terms of location, variability, shape, outliers. Numerical + graphical tools

summary(data$STARTING_PRICE)
var(data$STARTING_PRICE)
sd(data$STARTING_PRICE)

plot(data$STARTING_PRICE, pch = 8, col = "lightblue", ylab = "Starting price in millions SEK")
boxplot(data$STARTING_PRICE, col = "turquoise", horizontal = T, xlab = "Starting price in millions SEK")
hist(data$STARTING_PRICE, breaks = 35, col = "turquoise", xlab = "Starting Price in millions SEK", main = "Histogram of Starting price")

# Q2 - Analyze simultaneously REGION and TYPE

for (i in levels(data$REGION)) {
  region <- data[data$REGION == i, ]
  table <- round(prop.table(table(region$TYPE)), 2)
  cat(i)
  print(table)
  cat("\n")
}

par(mfrow = c(1, 3))
for (i in levels(data$TYPE)) {
  type_data <- data[data$TYPE == i, ]
  prop_table <- prop.table(table(type_data$REGION))
  barplot(prop_table, main = paste(i), xlab = "Region", ylab = "Proportion", names.arg = names(prop_table),
          col = c("yellow2", "pink3", "seagreen2", "lightsalmon2","turquoise"),
          ylim = c(0, 0.7))
}

# Q3 - Analyze simultaneously REGION and AREA

for (i in levels(data$REGION)) {
  region <- data[data$REGION == i, ]
  cat(i, "\n")
  print(summary(region$AREA))
  cat("\n")
}

par(mfrow = c(1,1))
boxplot(data$AREA ~ data$REGION, col = c("yellow2", "pink3", "seagreen2", "lightsalmon2","turquoise"), ylab = "Area", xlab = "Regions")

# Q4 - Analyze relationship between the variables STARTING_PRICE and AREA. 

cor(data$AREA, data$STARTING_PRICE)

plot(STARTING_PRICE ~ AREA, data = data, xlab = "Area", ylab = "Starting price in millions SEK", col = "turquoise", pch = 20, cex = 1)

# Q5 - Fit a linear regression explaining STARTING_PRICE by AREA.

lm_model <- lm(STARTING_PRICE ~ AREA, data = data)
summary(lm_model)
plot(STARTING_PRICE ~ AREA, data = data, xlab = "Area", ylab = "Starting price in millions SEK", col = "turquoise", pch = 20, cex = 1)
abline(lm_model, col = "red", lwd = 2)

# Q6 - Fit a linear regression explaining STARTING_PRICE by REGION, TYPE, BALCONY, ROOMS, AREA.

mr_model <- lm(STARTING_PRICE ~ REGION + TYPE + BALCONY + ROOMS + AREA, data = data)
summary(mr_model)

#  (Tips: i.Make use of appropriate numerical and graphical tools;ii.Make sure that you report the estimated regression in equation form;iii.Make sure toindicate how good are the independent variables at explaining the price;iv.Make sureyou interpret the coefficients of the regression.)7.  The  datasettest.xlsxthat  can  be  found  in  Athena  (Resources/Part  1/Assignment)contains information on the region, the type of housing unit, the presence of a balcony,the number of rooms and the area of ten housing units.  Use your regression model fromexercise 6 to predict the starting price of these housing units.









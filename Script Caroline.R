# This is Caroline's code



library(readxl)
test <- read_excel("test.xlsx")
data <- read_excel("dataset01.xlsx")
par(mfrow = c(1,1))

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
          col = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51"),
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
boxplot(data$AREA ~ data$REGION, col = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51"), ylab = "Area", xlab = "Regions", main = "Boxplot of Area by Region")

# Q4 - Analyze relationship between the variables STARTING_PRICE and AREA. 

cor(data$AREA, data$STARTING_PRICE)

plot(STARTING_PRICE ~ AREA, data = data, xlab = "Area", ylab = "Starting price in millions SEK", col = "turquoise", pch = 20, cex = 1)

# Q5 - Fit a linear regression explaining STARTING_PRICE by AREA.

lm_model <- lm(STARTING_PRICE ~ AREA, data = data)
?lm
summary(lm_model)
plot(STARTING_PRICE ~ AREA, data = data, xlab = "Area", ylab = "Starting price in millions SEK", col = "turquoise", pch = 20, cex = 1)
abline(lm_model, col = "red", lwd = 2)

# Q6 - Fit a linear regression explaining STARTING_PRICE by REGION, TYPE, BALCONY, ROOMS, AREA.

round(cor(data[, c("ROOMS", "AREA", "STARTING_PRICE")]), 3)
plot(AREA ~ ROOMS, data = data, xlab = "Rooms", ylab = "Area", col = "turquoise", pch = 20, cex = 1)
abline(lm(AREA ~ ROOMS, data = data), col = "red", lwd = 2)

mr_model <- lm(STARTING_PRICE ~ REGION + TYPE + BALCONY + ROOMS + AREA, data = data)
summary(mr_model)

# Improved MRM (remove rooms then balcony because insignificant)
mr_model2 <- lm(STARTING_PRICE ~ REGION + TYPE + AREA + REGION*AREA + TYPE*AREA, data = data)
summary(mr_model2)

par(mfrow = c(2, 3))

colours <- c("#99D492", "#188977", "#0A2F51")
for (i in levels(data$REGION)) {
    region <- data[data$REGION == i, ]
    
    plot(region$AREA, region$STARTING_PRICE,
         main = paste("Starting price vs area in", i),
         xlab = "Area", ylab = "Starting Price in millions SEK",
         pch = 19, col = colours[region$TYPE], cex = 1.5)
    
    legend("topleft", legend = c("Apartment", "Terrace", "Villa"), fill = colours, title = "Type", cex = 0.8)
}


# Q7 - Predict starting prices for dataset test

test_pred <- predict(mr_model, newdata = test)
test$STARTING_PRICE <- test_pred
head(test)







# This is Caroline's code

# Hola Patito Pablo and Big boss Steven and Fantastic Faezeh


library(readxl)
test <- read_excel("test.xlsx")
data <- read_excel("dataset01.xlsx")

# Q1 - Analyze STARTING_PRICE in terms of location, variability, shape, outliers. Numerical + graphical tools

data$STARTING_PRICE <- data$STARTING_PRICE / 1000000

summary(data$STARTING_PRICE)
var(data$STARTING_PRICE)
sd(data$STARTING_PRICE)

plot(data$STARTING_PRICE, pch = 8, col = "lightblue", ylab = "Starting price")
boxplot(data$STARTING_PRICE, col = "turquoise")
hist(data$STARTING_PRICE, breaks = 70, col = "turquoise", xlab = "Starting Price in millions SEK", main = "Histogram of Starting price")

# Q2 - Analyze simultaneously REGION and TYPE


# Q3 - Analyze simultaneously REGION and AREA


# Q4 - Analyze relationship between the variables STARTING_PRICE and AREA. 
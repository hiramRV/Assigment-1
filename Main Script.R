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
library(pacman)
p_load(openxlsx, mosaic, RColorBrewer, DescTools, e1071, readxl, tidyverse, ggplot2, scales, ggridges, janitor)

### Data Pre-processing ----
data <-read.xlsx("dataset01.xlsx")
data$REGION<-factor(data$REGION)
data$TYPE<-factor(data$TYPE)
data$BALCONY<-factor(data$BALCONY)
data$STARTING_PRICE <- data$STARTING_PRICE / 1000000
str(data)

test <-read.xlsx("test.xlsx")
str(test)
test$REGION<-factor(test$REGION)
test$TYPE<-factor(test$TYPE)
test$BALCONY<-factor(test$BALCONY)

### Question 1: ----
summary(data$STARTING_PRICE)
var(data$STARTING_PRICE)
sd(data$STARTING_PRICE)
skewness(data$STARTING_PRICE)

#Plots
boxplot(data$STARTING_PRICE, col = "turquoise", horizontal = T, xlab = "Starting price in millions SEK")
hist(data$STARTING_PRICE, breaks = 35, col = "turquoise", xlab = "Starting Price in millions SEK", main = "Histogram of Starting price")

### Question 2: ----
#Bar plots percentange
b_limpia <- data %>%
  clean_names()

b_limpia %>%
  count(type, region) %>%
  group_by(type) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  mutate(lab = paste0(round(por, digits = 1), "%"),
         col_por = ifelse(region == "West" | region == "Stockholm", "white", "black")) %>%
  ggplot(aes(type, por, fill = region)) +
  geom_bar(stat = "identity", color = "grey10") +
  geom_text(aes(type, label = lab, fontface = "bold", color = col_por), position = position_stack(vjust = 0.5), size = 4, show.legend = F) +
  scale_colour_manual(values=c("black", "white")) +
  theme_minimal() +
  xlab("Housing type") +
  ylab("Percentage of housing type by region") +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Region") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave("Outputs/col_type_region_1.png", width = 10, height = 10)

b_limpia %>%
  count(region, type) %>%
  group_by(region) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  mutate(lab = paste0(round(por, digits = 1), "%"),
         col_por = ifelse(type == "Villa", "white", "black")) %>%
  ggplot(aes(region, por, fill = type)) +
  geom_bar(stat = "identity", color = "grey10") +
  geom_text(aes(region, label = lab, fontface = "bold", color = col_por), position = position_stack(vjust = 0.5), size = 4, show.legend = F) +
  scale_colour_manual(values=c("black", "white")) +
  theme_minimal() +
  xlab("Region") +
  ylab("Percentage of housing by type") +
  scale_fill_discrete(type = c("#DEEDCF", "#1D9A6C", "#0E4D64")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Housing\ntype") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

ggsave("Outputs/col_type_region_2.png", width = 10, height = 10)


#Multiple bar plots proportion
par(mfrow = c(1, 3))
for (i in levels(data$TYPE)) {
  type_data <- data[data$TYPE == i, ]
  prop_table <- prop.table(table(type_data$REGION))
  barplot(prop_table, main = paste(i), xlab = "Region", ylab = "Proportion", names.arg = names(prop_table),
          col = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51"),
          ylim = c(0, 0.7))
}
#Contingency table
table(data$REGION, data$TYPE)

### Question 3: ----

#Box plot
for (i in levels(data$REGION)) {
  region <- data[data$REGION == i, ]
  cat(i, "\n")
  print(summary(region$AREA))
  cat("\n")
}
par(mfrow = c(1,1))
boxplot(data$AREA ~ data$REGION, col = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51"), ylab = "Area", xlab = "Regions", main = "Boxplot of Area by Region")

#Histograms
b_limpia %>%
  ggplot() +
  geom_density_ridges(aes(area, region, fill = region), alpha = .5, scale = 5) +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  theme_minimal() +
  labs(title = "Distribution of area by region",
       fill = "Region") +
  xlab("Area, in square meters") +
  ylab("Region")

ggsave("Outputs/area_density_by_region.png", width = 10, height = 10)

#IQR
for (i in levels(data$REGION)) {
  region_data <- data[data$REGION == i, ]
  IQR_R = quantile(region_data$STARTING_PRICE, 0.75) - quantile(region_data$STARTING_PRICE, 0.25) #IQR
  print(paste0("IQR in region ", i, " : ",IQR_R))
  
  print(paste0("Skewnes: ",round(skewness(region_data$STARTING_PRICE),digits=2)))
}


### Question 4: ----
print(paste0("Corelation between Starting price and Area: ",cor(data$AREA, data$STARTING_PRICE)))

#Per Region:
for (i in levels(data$REGION)) {
  data_region <- data[data$REGION == i, ]
  print(paste0("Region: ",i,". Cor Value: ",round(cor(data_region$AREA, data_region$STARTING_PRICE),digits=4) )) 
  group_reg = lm(STARTING_PRICE~AREA, data=data_region)
  print(paste0("Coefficients ",round(group_reg$coefficients[1],2)," ,",round(group_reg$coefficients[2],2)))
  print("")
}

#Per TYPE:
for (i in levels(data$TYPE)) {
  data_type <- data[data$TYPE == i, ]
  print(paste0("TYPE: ",i,". Cor Value: ",cor(data_type$AREA, data_type$STARTING_PRICE))) 
  
}

### Question 5: ----
reg_area_precio <- lm(starting_price ~ area, data = b_limpia)

summary(reg_area_precio)

b_limpia %>%
  ggplot() +
  geom_point(aes(area, starting_price, fill = region), shape = 21, color = "grey10", size = 2) +
  geom_abline(aes(slope = 46742, intercept = 826214), color = "salmon") +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  theme_minimal() +
  labs(title = "Starting price by Area",
       fill = "Region") +
  xlab("Area, in square meters") +
  ylab("Price") +
  scale_y_continuous(labels = label_comma())

ggsave("Outputs/simple_reg.png", width = 10, height = 10)
### Question 6: ----
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

#Plot

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


### Question 7: ----
p1<-1000000*predict(multiple_reg,test)   #Predict using first model
p2<-1000000*predict(multiple_reg6,test)   #Predict using last model model
#Differences
percent((p1 - p2) / p1)
p1-p2

### Assignment 1  
###
### Código elaborado por Pablo Paras Ochoa que es la mejor

### Paquetes & Setup ----
library(pacman)
p_load(readxl, tidyverse, ggplot2, janitor, e1071, scales, ggridges)

Sys.setlocale("LC_ALL", "es_ES.UTF-8") 
options(scipen=999) 

porc <- function(abs, pob) {
  (abs/pob)* 100
}

### Importar datos ----
b_raw <- read_excel("dataset01.xlsx")

b_limpia <- b_raw %>%
  clean_names()

### Pregunta 1 ----
r_p1 <-
b_limpia %>%
  summarise(promedio = mean(starting_price),
            mediana = median(starting_price),
            moda = mode(starting_price),
            rango = max(starting_price) - min(starting_price),
            q25 = quantile(starting_price, probs = c(.25)),
            q75 = quantile(starting_price, probs = c(.75)),
            iqr = IQR(starting_price),
            varianza = var(starting_price),
            des_stand = sd(starting_price),
            skew = skewness(starting_price))


b_limpia %>%
  ggplot() +
  geom_histogram(aes(starting_price), fill = "#188977") +
  geom_vline(aes(xintercept = mean(starting_price)), color = "salmon") +
  annotate("text", x = r_p1$promedio*1.3, y = 120, label = paste0("Mean: ", format(round(r_p1$promedio, digits = 0), big.mark = ",", scientific = FALSE) ,sep = "")) +
  theme_minimal() +
  xlab("Starting Price") +
  ylab("Count") +
  labs(title = "Histogram of the variable Starting Price",
       subtitle = "Using bins = 30") +
  scale_x_continuous(labels = label_comma())

b_limpia %>%
  ggplot() +
  geom_boxplot(aes(starting_price), fill = "grey90", color = "#188977") +
  theme_minimal() +
  xlab("Starting Price") +
  labs(title = "Boxplot of the variable Starting Price") +
  scale_x_continuous(labels = label_comma(),
                     breaks = c(seq(from = 2500000, to = 20000000, by = 2500000))) +
  scale_y_continuous(breaks = NULL) +
  theme(axis.text.x=element_blank(), 
        axis.ticks.x=element_blank()) +
  coord_flip() 

b_limpia %>%
  ggplot() +
  geom_density(aes(starting_price), fill = "grey90", color = "#188977") +
  geom_vline(aes(xintercept = mean(starting_price)), color = "salmon") +
  annotate("text", x = r_p1$promedio*1.4, y = .0000002, label = paste0("Mean: ", format(round(r_p1$promedio, digits = 0), big.mark = ",", scientific = FALSE) ,sep = "")) +
  theme_minimal() +
  xlab("Starting Price") +
  ylab("Density") +
  labs(title = "Distribution of the variable Starting Price") +
  scale_x_continuous(labels = label_comma())

### Pregunta 2 ----
b_limpia %>%
  count(type, region) %>%
  group_by(type) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(type, por, fill = region), position = "stack", color = "grey10") +
  theme_minimal() +
  xlab("Housing type") +
  ylab("Percentage of housing type by region") +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Region") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

b_limpia %>%
  count(region, type) %>%
  group_by(region) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(region, por, fill = type), position = "stack", color = "grey10") +
  theme_minimal() +
  xlab("Region") +
  ylab("Percentage of housing by type") +
  scale_fill_discrete(type = c("#DEEDCF", "#1D9A6C", "#0E4D64")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Housing\ntype") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 
  

b_limpia %>%
  count(type, region) %>%
  group_by(type) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(type, n, fill = region), position = "dodge", color = "grey10") +
  theme_minimal() +
  xlab("Housing type") +
  ylab("Percentage of housing type by region") +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Region") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 

b_limpia %>%
  count(region, type) %>%
  group_by(region) %>%
  mutate(tot = sum(n),
         por = n/tot*100) %>%
  ungroup() %>%
  ggplot() +
  geom_col(aes(region, n, fill = type), position = "dodge", color = "grey10") +
  theme_minimal() +
  xlab("Region") +
  ylab("Percentage of housing by type") +
  scale_fill_discrete(type = c("#0E4D64", "#1D9A6C", "#DEEDCF")) +
  labs(title = "Percentage of housing type in each region",
       fill = "Housing\ntype") +
  theme(axis.ticks.x=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) 










### Pregunta 3 ----
b_limpia %>%
  ggplot() +
  geom_density_ridges(aes(area, region, fill = region), alpha = .5, scale = 5) +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  theme_minimal() +
  labs(title = "Distribution of area by region",
       fill = "Region") +
  xlab("Area, in square meters") +
  ylab("Region")

### Pregunta 4 ----
b_limpia %>%
  ggplot() +
  geom_point(aes(area, starting_price, fill = region), shape = 21, color = "grey10", size = 2) +
  scale_fill_discrete(type = c("#DEEDCF", "#99D492", "#1D9A6C", "#188977", "#0A2F51")) +
  theme_minimal() +
  labs(title = "Starting price by Area",
       fill = "Region") +
  xlab("Area, in square meters") +
  ylab("Price") +
  scale_y_continuous(labels = label_comma())

b_limpia %>%
  summarise(cor = cor(area, starting_price))

### Pregunta 5 ----
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

### Pregunta 6 ----
reg_full <- lm(starting_price ~ area + region + type + balcony + rooms, data = b_limpia)

summary(reg_full)







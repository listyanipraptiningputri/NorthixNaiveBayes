# Libraries
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)

# Data
data <- Northix.csv(file.choose(), header = T)
str(data)
xtabs(~Left-Weight+Right-Distance, data = data)
data$Right-Distance <- as.factor(data$Right-Distance)
data$Left-Weight <- as.factor(data$Left-Weight)

# Visualization
pairs.panels(data[-1])
data %>%
  ggplot(aes(x=Left-Weight, y=Right-Weight, fill = Left-Weight)) +
  geom_boxplot() +
  ggtitle("Box Plot")

data %>% ggplot(aes(x=Right-Weight, fill = Left-Weight)) +
  geom_density(alpha=0.8, color= 'black') +
  ggtitle("Density Plot")

# Data Partition
set.seed(1234)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind == 1,]
test <- data[ind == 2,]

# Naive Bayes Model
model <- naive_bayes(Left-Weight ~ ., data = train, usekernel = T)
model

train %>%
  filter(Left-Weight == "1") %>%
  summarise(mean(Left-Distance), sd(Left-Distance))

plot(model)

# Predict
p <- predict(model, train, type = 'prob')
head(cbind(p, train))

# Confusion Matrix - train data
p1 <- predict(model, train)
(tab1 <- table(p1, train$Left-Weight))
1 - sum(diag(tab1)) / sum(tab1)

# Confusion Matrix - test data
p2 <- predict(model, test)
(tab2 <- table(p2, test$Left-Weight))
1 - sum(diag(tab2)) / sum(tab2)

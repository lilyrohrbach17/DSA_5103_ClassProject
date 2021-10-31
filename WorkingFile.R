library(mlbench)
library(dplyr)
library(ibd)
library(tidyverse)
library(reshape2)
library(datasets)
library(devtools)
library(scatterplot3d)
library(rgl)
library(ggplot2)
library(ggbiplot)
library(caret)
library(MASS)
library(HSAUR2)
library(outliers)

train <- read_csv('fraudTrain.csv')
test <- read_csv('fraudTest.csv')

attach(train)

#Creat a subset of train that only has numeric columns
num_cols <- unlist(lapply(train, is.numeric)) 
numericTrain <- train[ , num_cols]
 
#Create matrix of correlations for the numeric variables in the
#data set.
corMat = cor(numericTrain)
meltedCorMat <- melt(corMat)
meltedCorMat$value <- round(meltedCorMat$value, 3)

#Correlation Matrix plot.

#Notes: Fraud appears to be most correlated with amount in regards
#to the numeric variables

#From the discussions on Kaggle, a few new features could be created with 
#better correlation: Time Since Last Transaction per card, Frequency of 
#transactions in last X days, and encoding a transaction as abnormal
#based on time of day
ggplot(data = as.data.frame(meltedCorMat), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) + 
  scale_fill_gradient2(name="Correlation") +
  labs(title = "Correlation Between Elements in Training Data") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks = element_blank())


#Convert Fraud to Categorical and get subset of only the
#fradulent transactions
train$is_fraud <- as.factor(train$is_fraud)
trainFraud <- train[train$is_fraud == 1, ]

#Plot of Fraud by Category
ggplot(data = trainFraud, aes(x=category)) +
  geom_bar()  +
  labs(title = "Distribution of Fradulent Transactions by Category") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks = element_blank())

#Plot of Fraud by State
#Note: Probably want to normalize this by dividing by the 
#state population if available
ggplot(data = trainFraud, aes(x=state)) +
  geom_bar()  +
  labs(title = "Distribution of Fradulent Transactions by State") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks = element_blank())

#Plot of Fraud by Gender 
#Note: Almost Equal. Male is slightly higher
ggplot(data = trainFraud, aes(x=gender)) +
  geom_bar()  +
  labs(title = "Distribution of Fradulent Transactions by Gender") + 
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
    axis.ticks = element_blank())



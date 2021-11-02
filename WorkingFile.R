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

#Boxplots for Outlier Identification
ggplot(train, aes(x = is_fraud, y = dob)) +
  geom_boxplot()
#Appears to be an outlier for real transactions based on ewarlier DOB

ggplot(train, aes(x = gender, y = amt, color = is_fraud)) +
  geom_boxplot()
#Both males and females tend to have similar spending habits with high value outliers 

ggplot(train, aes(x = merch_lat, y = lat, color = is_fraud)) +
  geom_boxplot()
#Similar latitudes between the customer and the merchant for
#fraud and real transactions with some higher outliers and a few lower 

ggplot(train, aes(x = merch_long, y = long, color = is_fraud)) +
  geom_boxplot()
#Similar longitudes between the customer and the merchant for
#fraud and real transactions with some lower outliers

#Outlier elimination
#merch_long
Q <- quantile(train$merch_long, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$merch_long)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, merch_long > 
                      (Q[1] - 1.5*iqr) & merch_long < (Q[2]+1.5*iqr))
#long
Q <- quantile(train$long, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$long)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, long > 
                      (Q[1] - 1.5*iqr) & long < (Q[2]+1.5*iqr))
#merch_lat
Q <- quantile(train$merch_lat, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$merch_lat)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, merch_lat > 
                      (Q[1] - 1.5*iqr) & merch_lat < (Q[2]+1.5*iqr))
#lat
Q <- quantile(train$lat, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$lat)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, lat > 
                      (Q[1] - 1.5*iqr) & lat < (Q[2]+1.5*iqr))
#amt
Q <- quantile(train$amt, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$amt)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, amt > 
                      (Q[1] - 1.5*iqr) & amt < (Q[2]+1.5*iqr))
#dob
Q <- quantile(train$dob, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(train$dob)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range﻿

eliminated<- subset(train, dob > 
                      (Q[1] - 1.5*iqr) & dob < (Q[2]+1.5*iqr))
#Error not defined for "Date" Objects

#Factor Collapsing
train$job_c<-fct_collapse(train$job,
                            Accountant=c("Accountant, chartered public finance", "Chartered public finance accountant",
                                         "Accountant, chartered certified", "Chartered accountant", "Accountant, chartered"),
                            Therapist=c("Dance movement psychotherapist","Therapist, occupational", "Physiotherapist",
                                        "Child psychotherapist", "Therapist, horticultural", "Therapist, sports", 
                                        "Therapist, occupational", "Phytotherapist", "Psychotherapist, child", "Physiotherapist", 
                                        "Art therapist", "Music therapist", "Occupational therapist", "Therapist, sports",
                                        "Horticultural therapist", "Therapist, drama", "Therapist, art", "Nutritional therapist"),
                            Psychologist=c("Psychologist, counselling","Forensic psychologist","Psychologist, forensic", 
                                           "Educational psychologist", "Counselling psychologist", "Occupational psychologist", 
                                           "Psychologist, sport and exercise", "Clinical psychologist", "Psychologist, clinical", 
                                           "Sport and exercise psychologist"),
                            Administrator=c("Administrator, education","Administrator, charities/voluntary organisations", 
                                            "Database administrator", "Administrator", "Administrator, local government", 
                                            "Secretary/administrator", "Education administrator" , "Civil Service administrator", 
                                            "Sports administrator", "Administrator, arts"))



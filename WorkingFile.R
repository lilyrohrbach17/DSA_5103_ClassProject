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



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

# Remove % of test data
newtrain = train %>% 
  group_by(is_fraud) %>% 
  sample_frac(size = 0.001)

# Remove % of test data
newtest = test %>% 
  group_by(is_fraud) %>% 
  sample_frac(size = 0.001)

test<-newtest
train<-newtrain
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
train$job<-fct_collapse(train$job,
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
                                          "Sports administrator", "Administrator, arts"),
                          Designer = c("Designer, multimedia", "Designer, jewellery", "Product designer", 
                                       "Exhibition designer", "Designer, ceramics/pottery", 
                                       "Designer, interior/spatial", "Jewellery designer", 
                                       "Designer, industrial/product", "Industrial/product designer", 
                                       "Ceramics designer",  "Web designer", "Designer, furniture",
                                       "Furniture designer", "Interior and spatial designer", 
                                       "Designer, exhibition/display", "Textile designer", 
                                       "Designer, television/film set", "Designer, textile", 
                                       "Glass blower/designer"), 
                          Engineer = c("Engineer, land", "Energy engineer", 'Network engineer', 
                                       "Building services engineer", "Electrical engineer", "Engineer, technical sales", 
                                       "Engineer, electronics", "Water engineer", "Geologist, engineering", 
                                       "Engineer, broadcasting (operations)", "Engineer, biomedical", 
                                       "Engineering geologist", "Mining engineer", "Engineer, communications", 
                                       "Materials engineer", "Engineer, structural", "Structural engineer", 
                                       "Mechanical engineer", "Electronics engineer", "Chemical engineer", 
                                       "Engineer, building services", "Engineer, mining", "Control and instrumentation engineer", 
                                       "Engineer, control and instrumentation", "Engineer, maintenance", 
                                       "Engineer, production", "Manufacturing engineer", "Production engineer", 
                                       "Engineer, drilling", "Engineer, petroleum", "Civil engineer, contracting", 
                                       "Engineer, agricultural", "Engineer, manufacturing", "Engineer, automotive", 
                                       "Site engineer", "Manufacturing systems engineer", "Petroleum engineer"), 
                          Scientist = c("Scientist, research (maths)", "Product/process development scientist", 
                                        "Research scientist (physical sciences)", "Audiological scientist", 
                                        "Scientist, audiological", "Physiological scientist", "Scientist, marine", 
                                        "Research scientist (life sciences)", "Water quality scientist", 
                                        "Scientist, physiological", "Scientist, biomedical", "Geoscientist", 
                                        "Soil scientist", "Data scientist", "Scientist, research (medical)", 
                                        "Scientist, clinical (histocompatibility and immunogenetics)", 
                                        "Research scientist (medical)"), 
                          Teacher = c("Special educational needs teacher", "English as a second language teacher", 
                                      "Teacher, English as a foreign language", "Teacher, early years/pre", 
                                      "Primary school teacher", "Secondary school teacher", "Teacher, secondary school", 
                                      "Teacher, special educational needs", "Early years teacher", "Private music teacher", 
                                      "Teacher, primary school", "TEFL teacher", "Teacher, adult education"))
Full<-train%>%rbind(test)
#GLM model
GLMfit <- glm(data=Full,is_fraud ~ job + merch_lat + 
                merch_long + lat + long + amt + dob, family="binomial")

summary(GLMfit, style="pmax")


GLMTarget<-predict(GLMfit,test)
GLMdf<-aggregate(GLMTarget, by= list(test$trans_num),first)
GLMdf<-GLMdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(LMdf,"GLMPredictions.csv")

#LM Model
LMfit <- lm(data=Full,is_fraud ~ job + merch_lat + 
              merch_long + lat + long + amt + dob)

summary(LMfit, style="pmax")

LMTarget<-predict(LMfit,test)
LMdf<-aggregate(LMTarget, by= list(test$trans_num),first)
LMdf<-LMdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(LMdf,"LMPredictions.csv")

#PCR Model
PCRfit <- plsr(data=Full,is_fraud ~ job + merch_lat + 
                 merch_long + lat + long + amt + dob, scale=TRUE, validation="CV")

summary(PCRfit, style="pmax")


PCRTarget<-predict(PCRfit,test)
PCRdf<-aggregate(PCRTarget, by= list(test$trans_num),first)
PCRdf<-PCRdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(PCRdf,"PCRPredictions.csv")


#Random Forest Model
RFfit<- randomForest(data=Full,is_fraud ~ + merch_lat + 
                       merch_long + lat + long + amt + dob,
                     importance=TRUE,
                     prOximity=TRUE,
                     na.action=na.omit)
summary(RFfit, style="pmax")


RFTarget<-predict(RFfit,test)
RFdf<-aggregate(RFTarget, by= list(test$trans_num),first)
RFdf<-RFdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(RFdf,"RandomForestsPredictions.csv")

#MARS Model
Mfit<- earth(is_fraud ~ job + merch_lat + merch_long + lat + 
               long + amt + dob,data=Full)
summary(Mfit, style="pmax")


MTarget<-predict(Mfit,test)
Mdf<-aggregate(MTarget, by= list(test$trans_num),first)
Mdf<-Mdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(Mdf,"MARSPredictions.csv")

#Ridge Regression Model

# Possible ranges of lambda
lambda <- 10^seq(-10, 10, length.out = 200)

# Implement ridge regression with 5-fold cv and alpha=0
Ridgefit <- train(is_fraud ~ job + merch_lat + 
                      merch_long + lat + long + amt + dob, data=Full,
                    preProcess=c("center", "scale"), method="glmnet", 
                    metric="RMSE", na.action=na.omit,
                    tuneGrid=expand.grid(alpha=0, lambda=lambda),
                    trControl=trainControl(method="cv", number=5))

Ridgefit$bestTune 
Ridgefit$results %>% filter(lambda == Ridgefit$bestTune$lambda)

round(coef(Ridgefit$finalModel, Ridgefit$bestTune$lambda),3)
Ridgefit$bestTune$lambda
summary(Ridgefit, style="pmax")
Ridgefit

RidgeTarget<-predict(Ridgefit,test)
Ridgedf<-aggregate(RidgeTarget, by= list(test$trans_num),first)
Ridgedf<-Ridgedf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(Ridgedf,"RidgePredictions.csv")

#LASSO model
cvSpecification <- trainControl(method="cv", number=5,
                                savePredictions="all")

# Create vector of potential lambda values
Llambda <- 10^seq(5, -5, length=200)
LASSOfit <- train(is_fraud ~ merch_lat + 
                    merch_long + lat + long + amt + dob, data=Full,
                  preProcess=c("center", "scale"),
                  method="lasso", 
                  metric="RMSE",
                  trControl=cvSpecification,
                  na.action=na.omit)

# Best/optimal tuning hyperparameters (alpha, lambda)
LASSOfit$bestTune        # best tune 
LASSOfit$bestTune$Llambda
LASSOfit$results %>% filter(Llambda == LASSOfit$bestTune$Llambda)

summary(LASSOfit, style="pmax")
RMSE(Train$totRevenue, LASSOfit$residuals)

LASSOTarget<-predict(LASSOfit,Test)
LASSOdf<-aggregate(LASSOTarget, by= list(test$trans_num),first)
LASSOdf<-LASSOdf %>% mutate_if(is.numeric, ~ceiling(.))
write.csv(LASSOdf,"LASSOPredictions.csv")

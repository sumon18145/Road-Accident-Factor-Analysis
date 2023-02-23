#Load the packages
library(dplyr)  
library(ggplot2)
library(tidyr)
library(stats)

# load the CSV file from the local directory

dataset <- read.csv("C:/Users/PC/OneDrive/Desktop/Project/Road Accident/Road accident dataset.csv")

# column names of the data set

colnames(dataset)

#list types for each attribute

sapply(dataset, class)

# display the data

head(dataset,n=10)

#Managing data
dataset$Speeds.catagory <- ifelse(dataset$Driving.speeds < 45,'General Speeds','High Speeds')

#classify the variables into categorical and numerical variables

#select the numerical variables

numeric_var <-dataset %>% 
  select("Age","Driving.speeds","Driving.time.day","Driving.distence.day")

#select the categorical values


categorical_var<- dataset %>%
  select("Age.group","Gender","Education","Vehicle.type","Experience","Driving.license","Follow.traffic.rule",
         "Vehicle.fitness","Speeds.catagory","Using.safety.instruments","Mobile.using.habit","Overtaking.habit","Facing.accident")%>%
  mutate_if(is.numeric, as.factor)

#combine the categorical  and numerical values

dataset.r.a = cbind(categorical_var,numeric_var)

#Dimensions of the datasets:

dim(dataset.r.a)
sapply(dataset.r.a, class)

#visualization of data:
#Bar plot of Gender
ggplot(dataset.r.a, aes(x = Gender, fill = factor(Gender))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Grnder") +
  ggtitle("Gendr Distribution") +
  scale_fill_discrete(labels=c('1-Male', '2-Female')) +
  theme_classic()

#Bar plot of Age
ggplot(dataset.r.a, aes(x = Age.group, fill = factor(Age.group))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Age_group") +
  ggtitle("Age Distribution") +
  scale_fill_discrete(labels=c('1-Young Age', '2-Middle Age', '3-Old Age')) +
  theme_classic()

#Bar plot of Vehicle Type driven by the respondent.
ggplot(dataset.r.a, aes(x = Vehicle.type, fill = factor(Vehicle.type))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30) +
  xlab("Vehicle Type driven by the respondent") +
  ggtitle("Vehicle Type Distribution") +
  scale_fill_discrete(labels=c('1-Auto rickshaw', '2-Motor cycle', '3-Private car','4-Bus/Truck')) +
  theme_classic()

#Bar plot of Driving Experience.
ggplot(dataset.r.a, aes(x = Experience, fill = factor(Experience))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Driving Experience") +
  ggtitle("Driving Experience Distribution") +
  scale_fill_discrete(labels=c('1-Less then 1 year', '2-1 to 5 years', '3-5 t0 10 years','4-More than 10 years')) +
  theme_classic()

#Bar plot of Driving License:.
ggplot(dataset.r.a, aes(x = Driving.license, fill = factor(Driving.license))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Driving License") +
  ggtitle("Driving License Distribution") +
  scale_fill_discrete(labels=c('1-Yes', '2-No'))+
  theme_classic() 

#Bar plot of Traffic Rules:
ggplot(dataset.r.a, aes(x = Follow.traffic.rule, fill = factor(Follow.traffic.rule))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Traffic Rules") +
  ggtitle("Traffic Rules Distribution") +
  scale_fill_discrete(labels=c('1-Yes', '2-No')) +
  theme_classic() 

#Bar plot of Respondent facing an accident:
ggplot(dataset.r.a, aes(x = Facing.accident, fill = factor(Facing.accident))) +
  geom_bar() +
  geom_text(stat='count',aes(label=..count..),vjust=-0.30)+
  xlab("Respondent facing an accident") +
  ggtitle("Traffic Rules Distribution") +
  scale_fill_discrete(labels=c('1-Yes', '2-No')) +
  theme_classic() 

#Cross Table analysis:
#Load the packages
library(sjPlot)
#Analysis

#Association between Driving License and Facing Accident:
tab_xtab(var.row = dataset.r.a$Facing.accident , var.col = dataset.r.a$Driving.license, title = "Association between Driving License and Facing Accident", show.row.prc = TRUE)

#Association between Overtaking habit and Facing Accident:
tab_xtab(var.row = dataset.r.a$Facing.accident , var.col = dataset.r.a$Overtaking.habit , title = "Association between Overtaking habit and Facing Accident", show.row.prc = TRUE)

#Association between Following traffic rules and Facing Accident:
tab_xtab(var.row = dataset.r.a$Facing.accident , var.col = dataset.r.a$Follow.traffic.rule , title = "Association between Following traffic rules and Facing Accident", show.row.prc = TRUE)

#Association between Experience and Facing Accident:
tab_xtab(var.row = dataset.r.a$Facing.accident , var.col = dataset.r.a$Experience , title = "Association between Experience and Facing Accident", show.row.prc = TRUE)  

#Association between Regular Speeds and Facing Accident:
tab_xtab(var.row = dataset.r.a$Facing.accident , var.col = dataset.r.a$Speeds.catagory, title = "Association between Regular Speeds and Facing Accident", show.row.prc = TRUE) 

#Binary Logistic Regression:


#Generalized Linear Model fitting. Logistic regression is type of GLM
road.accident <- glm(Facing.accident ~ Follow.traffic.rule+Experience+Driving.license+Speeds.catagory+Overtaking.habit, family=binomial,data=dataset.r.a)
summary(road.accident)
#Interpretation:
#Since p-value is < 0.05 for Experience3 then these independent variable are significant and sign of the coefficients are also logical.

#Find odds ratio:
x <- coef(road.accident) #identify the model coefficients.
odd_ratio <- exp(coef(road.accident)) #find odds ratio.
y <- exp(confint(road.accident)) #calculates confidence interval for odds ratio.
OddRatio <- cbind(x, odd_ratio, y)
OddRatio
#Interpretation:
#The odd ratio for "Overtaking.habit2" is approximately 2.11
#For one unit change "Overtaking.habit2", the odds of being an Accident will change by 2.11 folds. 

#Predicting Probabilities
dataset.r.a$predprob<-round(fitted(road.accident),2)
head(data,n=10)
# Last column in the data 'predprob' is the probabilities generated using final model

#Classification and Sensitivity and Specificity table

#Classification:
classificationtable<-table(dataset.r.a$Facing.accident, dataset.r.a$predprob > 0.5)
classificationtable 
#Interpretation:
#True indicates predicted accident and False indicates predicted non-accident
#There are 42 correctly predicted accident and 17 correctly predicted non-accident.
#There are 20 wrongly predicted as accident and 41 wrongly predicted as non-accident. 

# Sensitivity and Specificity
sensitivity<-(classificationtable[2,2]/(classificationtable[2,2]+classificationtable[2,1]))*100
sensitivity

specificity<-(classificationtable[1,1]/(classificationtable[1,1]+classificationtable[1,2]))*100
specificity
#Interpretation:
#The Sensitivity is at 70.69% and the Specificity is at 67.74% . This is when the cutoff was set at 0.5


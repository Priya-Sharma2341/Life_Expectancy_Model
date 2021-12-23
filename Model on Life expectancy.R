#Importing data
data=read.csv("C:/Users/Priya Sharma/Desktop/Data Science R Notes/Life Expectancy Data.csv")
data.frame(data)
View(data)
summary(data)
head(data)
str(data)


#Data splitting
library("caTools")
set.seed(150)
split=sample.split(data,SplitRatio = 0.80)
split
train1=subset(data,split=="TRUE")
test1=subset(data,split=="FALSE")
train1
test1

#Data Preprocessing
#Looking for missing values
#install.packages("naniar")
#install.packages('readr')
library(naniar)
library(readr)
colSums(is.na(data))
vis_miss(data) #To check missing values visually

#outlier check
boxplot(data$BMI)
boxplot(data$Life_expectancy)
boxplot(data$Adult.Mortality)
boxplot(data$Schooling)
boxplot(data$under.five.deaths)
boxplot(data$Total.expenditure)
boxplot(data$infant.deaths)
boxplot(data$percentage.expenditure)
boxplot(data$GDP)
boxplot(data$Population)
boxplot(data$Income.composition.of.resources)
boxplot(data$thinness.5.9.years)
boxplot(data$thinness..1.19.years)


#Dummy coding
dummy=ifelse(data$Status=="Developed",1,0)
dummy


#Data Preprocessing
#Looking for missing values
#install.packages("naniar")
#install.packages('readr')
library(naniar)
library(readr)
colSums(is.na(train1))
vis_miss(train1) #To check missing values visually

#NA values imputation 
sum(is.na(train1$Adult.Mortality))
hist(train1$Adult.Mortality)
mean(train1$Adult.Mortality,na.rm="TRUE")
median(train1$Adult.Mortality,na.rm="TRUE")
#It is clearly not normally distributed
#since it is not normally distributed it is ideal to replace missing values by median
train1$Adult.Mortality[is.na(train1$Adult.Mortality)]=median(train1$Adult.Mortality,na.rm="TRUE")
sum(is.na(train1$Adult.Mortality))
train1$Adult.Mortality


sum(is.na(train1$Alcohol))
hist(train1$Alcohol)
median(train1$Alcohol,na.rm="TRUE")
#Not normally distributed, it is positively skewed
train1$Alcohol[is.na(train1$Alcohol)]=median(train1$Alcohol,na.rm="TRUE")
sum(is.na(train1$Alcohol))


sum(is.na(train1$Hepatitis.B))
hist(train1$Hepatitis.B)
median(train1$Hepatitis.B,na.rm="TRUE")
#Not normally distributed, it is negatively skewed
train1$Hepatitis.B[is.na(train1$Hepatitis.B)]=median(train1$Hepatitis.B,na.rm="TRUE")
sum(is.na(train1$Hepatitis.B))


sum(is.na(train1$Total.expenditure))
hist(train1$Total.expenditure)
median(train1$Total.expenditure,na.rm="TRUE")
mean(train1$Total.expenditure,na.rm="TRUE")
#Not normally distributed 
train1$Total.expenditure[is.na(train1$Total.expenditure)]=median(train1$Total.expenditure,na.rm="TRUE")
sum(is.na(train1$Total.expenditure))


sum(is.na(train1$Population))
hist(train1$Population)
median(train1$Population,na.rm="TRUE")
#Not normally distributed 
train1$Population[is.na(train1$Population)]=median(train1$Population,na.rm="TRUE")
sum(is.na(train1$Population))


sum(is.na(train1$GDP))
hist(train1$GDP)
median(train1$GDP,na.rm='TRUE')
#Not normally distributed
train1$GDP[is.na(train1$GDP)]=median(train1$GDP,na.rm='TRUE')
sum(is.na(train1$GDP))


sum(is.na(train1$BMI))
hist(train1$BMI)
median(train1$BMI,na.rm="TRUE")
#Not normally distributed, it is positively skewed
train1$BMI[is.na(train1$BMI)]=median(train1$BMI,na.rm="TRUE")
sum(is.na(train1$BMI))


sum(is.na(train1$Schooling))
hist(train1$Schooling)
median(train1$Schooling,na.rm="TRUE")
mean(train1$Schooling,na.rm="TRUE")
#Not normally distributed
train1$Schooling[is.na(train1$Schooling)]=median(train1$Schooling,na.rm="TRUE")
sum(is.na(train1$Schooling))


sum(is.na(train1$Diphtheria))
hist(train1$Diphtheria)
median(train1$Alcohol,na.rm="TRUE")
#Not normally distributed, it is negatively skewed
train1$Diphtheria[is.na(train1$Diphtheria)]=median(train1$Diphtheria,na.rm="TRUE")
sum(is.na(train1$Diphtheria))


sum(is.na(train1$thinness..1.19.years))
hist(train1$thinness..1.19.years)
median(train1$thinness..1.19.years,na.rm="TRUE")
#Not normally distributed, it is positively skewed
train1$thinness..1.19.years[is.na(train1$thinness..1.19.years)]=median(train1$thinness..1.19.years,na.rm="TRUE")
sum(is.na(train1$thinness..1.19.years))


sum(is.na(train1$thinness.5.9.years))
hist(train1$thinness.5.9.years)
median(train1$thinness.5.9.years,na.rm="TRUE")
#Not normally distributed, it is positively skewed
train1$thinness.5.9.years[is.na(train1$thinness.5.9.years)]=median(train1$thinness.5.9.years,na.rm="TRUE")
sum(is.na(train1$thinness.5.9.years))


sum(is.na(train1$Polio))
hist(train1$Polio)
median(train1$Polio,na.rm="TRUE")
#Not normally distributed, it is negatively skewed
train1$Polio[is.na(train1$Polio)]=median(train1$Polio,na.rm="TRUE")
sum(is.na(train1$Polio))


sum(is.na(train1$Life_expectancy))
hist(train1$Polio)
median(train1$Life_expectancy,na.rm="TRUE")
#Not normally distributed, it is negatively skewed
train1$Life_expectancy[is.na(train1$Life_expectancy)]=median(train1$Life_expectancy,na.rm="TRUE")
sum(is.na(train1$Life_expectancy))



sum(is.na(train1$Income.composition.of.resources))
hist(train1$Income.composition.of.resources)
median(train1$Income.composition.of.resources,na.rm="TRUE")
#Not normally distributed, it is positively skewed
train1$Income.composition.of.resources[is.na(train1$Income.composition.of.resources)]=median(train1$Income.composition.of.resources,na.rm="TRUE")
sum(is.na(train1$Income.composition.of.resources))


vis_miss(train1)

#install.packages("corrplot")
library(corrplot)
isnumeric=which(sapply(data,is.numeric))
names(isnumeric)
numvars=data[,isnumeric]
str(numvars)
cor(numvars,use="pairwise.complete.obs") #pairwise.complete.obs helps exclude NA correlation
cor_numvars=round(cor(numvars,use="pairwise.complete.obs"),2) #correlation will come in only 2 decimal points/improvising the code)
cor_numvars
write.csv(cor_numvars,"Correlation.csv") #Creates a CSV file for correlation between numeric variables
corrplot.mixed(cor_numvars,tl.col="black",tl.pos="lt")


#Model Building 
model1=lm(Life_expectancy~.,data=train1)
model1
length(model1)
summary(model1)
plot(model1)

#Checking for multicolinearity
library(car)
vif(model1)
alias(model1)
train1$Country=NULL
data$Country=NULL

#Rerun model1 before running the below function

vif(model1)

#Prediction on train
pd=predict(model1,data=train1)
pd
predicted_life_expectancy=pd
predicted_life_expectancy
length(predicted_life_expectancy)


actual_life_expectancy=train1$Life_expectancy
actual_life_expectancy
length(actual_life_expectancy)

mean(abs((actual_life_expectancy - predicted_life_expectancy)/actual_life_expectancy))*100


#Find Residual/Error
#error=actual-predicted
#RMSE
sqrt(mean((actual_life_expectancy - predicted_life_expectancy)^2))
difference=resid(model1)
length(difference)
#Creating a Data Frame for actual_life_expectancy, predicted_life_expectancy
                                                             #and difference
df_life_expectancy=data.frame(actual_life_expectancy, predicted_life_expectancy,difference)
View(df_life_expectancy)
write.csv(df_life_expectancy,"Difference_Life_Expectancy.csv")
pred_test=predict(model1,data=test1)
pred_test
length(pred_test)

#install.packages("broom")
library(broom)
#To organise the model we use the function tidy
tidy_model1=tidy(model1)
tidy_model1

install.packages("MLmetrics")
library(MLmetrics)

MAPE(df_life_expectancy$predicted_life_expectancy,df_life_expectancy$actual_life_expectancy)

install.packages("forecast")
library(forecast)
accuracy(predicted_life_expectancy, actual_life_expectancy)



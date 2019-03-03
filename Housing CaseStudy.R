##########################################################################################################################
#                                                                                                                        #
#                                                                                                                        #
#                                              Linear Regression                                                         #
#                                              Housing  Example                                                          #
#                                                                                                                        #
#                                                                                                                        #
##########################################################################################################################
#-------------------------------------------------------------------------------------------------------------------------
#Import necessary packages into the code ground
#-------------------------------------------------------------------------------------------------------------------------
library(caret)
library(corrplot)
library(caret)
library(car)
#-------------------------------------------------------------------------------------------------------------------------

#Import the nessary data in the code ground
#-------------------------------------------------------------------------------------------------------------------------
Housing <- read.csv("C:/Users/dell/Downloads/Analytix/Micky BTCMP/R lang/Stat/Boot Camp - 27th August 2018 - Linear Regression/Case Study - Housing Example/House_Prices.csv")
#-------------------------------------------------------------------------------------------------------------------------

#understanding how the data is 
#Study the data and the which type of variables are present and how many levely are present in each variables 
#-------------------------------------------------------------------------------------------------------------------------
str(Housing)
psych::describe(Housing)
#-------------------------------------------------------------------------------------------------------------------------
numeric_vars <- names(Housing)[sapply(Housing, FUN=is.numeric)]
cat_vars = names(Housing)[sapply(Housing, FUN=is.factor)]


#Seeing the correletion with in gthe x variables
#-------------------------------------------------------------------------------------------------------------------------
h1 <- cor(Housing[numeric_vars])
h1
#-------------------------------------------------------------------------------------------------------------------------
#Checking the outliers in each variables 
#if there are any outliers are present then outlier treatments should be done for the variables
#belove steps for find the outliers in the each variables 
#outlier treatmment only for numerical variables
#-------------------------------------------------------------------------------------------------------------------------
Housing2 <- Housing
boxplot(Housing2$Price)
Housing2$Price <- NULL  #Price Contains Outliers
boxplot(Housing2$SqFt)
Housing2$SqFt <- NULL  # SqFt Contains outliers
boxplot(Housing2$Home)
Housing2$Home <- NULL  # No Outliers
boxplot(Housing2$Bedrooms)
Housing2$Bedrooms <- NULL # Bedrooms Contains outliers
boxplot(Housing2$Offers)
Housing2$Offers <- NULL  #Offers has null outliers
boxplot(Housing2$Bathrooms)
Housing2$Bathrooms <- NULL # No outliers
drop(Housing2)
#-------------------------------------------------------------------------------------------------------------------------
#User defined function to create Data audit report for numerical variables
#-------------------------------------------------------------------------------------------------------------------------
mystats_num = function(x){
  n = length(x)
  nmiss = sum(is.na(x))
  nmiss_pct = mean(is.na(x))
  sum = sum(x, na.rm=T)
  mean = mean(x, na.rm=T)
  median = quantile(x, p=0.5, na.rm=T)
  std = sd(x, na.rm=T)
  cv = sd(x, na.rm=T)/mean(x, na.rm=T)
  var = var(x, na.rm=T)
  range = max(x, na.rm=T)-min(x, na.rm=T)
  pctl = quantile(x, p=c(0, 0.01, 0.05,0.1,0.25,0.5, 0.75,0.9,0.95,0.99,1), na.rm=T)
  return(c(N=n, Nmiss =nmiss, Nmiss_pct = nmiss_pct, sum=sum, avg=mean, meidan=median, std=std, cv=cv, var=var, range=range, pctl=pctl))
}
#-------------------------------------------------------------------------------------------------------------------------
#User defined function to create Data audit report for categorical variables
#-------------------------------------------------------------------------------------------------------------------------
mystats_cat = function(x){
  Var_Type=class(x)
  n<-length(x)
  nmiss<-sum(is.na(x))
  fre<-list(table(x))
  prop<-list(prop.table(table(x)))
  #x[is.na(x)]<-x[which.max(prop.table(table(x)))]
  return(c(Var_Type=Var_Type, n=n,nmiss=nmiss,freq=fre,proportion=prop))
}
#-------------------------------------------------------------------------------------------------------------------------
numeric_vars <- names(Housing)[sapply(Housing, FUN=is.numeric)]
Housing_num<- Housing[,(numeric_vars)]
cat_vars = names(Housing)[sapply(Housing, FUN=is.factor)]
#-------------------------------------------------------------------------------------------------------------------------
#checking wheath all the numeriacl variables are following the normal disturbution or not 
#by taking the histogram we can observe the which disturbution is the variable is following 

#-------------------------------------------------------------------------------------------------------------------------
hist(Housing$Price)
#here the y variable is not a noraml disturbution 
#it is in the form of near normal distubution 

#-------------------------------------------------------------------------------------------------------------------------
# Q3_Price <- quantile(Housing$Price, 0.75)
# Q1_Price <- quantile(Housing$Price, 0.25)
# IQR_Price <- Q3_Price-Q1_Price
# 
# Uc_rice <- Q3_Price+1.5*IQR_Price
# Lc_Price <- Q1_Price-1.5*IQR_Price
#Outlier treatment
#-------------------------------------------------------------------------------------------------------------------------
# outlier_treat <- function(x){
#   # UC1 = quantile(x, p=0.90,na.rm=T)
#   # LC1 = quantile(x, p=0.10,na.rm=T)
#   # UC1 = mean(x,na.rm=T) + 3*sd(x, na.rm=T)
#   # LC1 = mean(x,na.rm=T) - 3*sd(x, na.rm=T)
#   Q3 <- quantile(x, 0.75)
#   Q1 <- quantile(x, 0.25)
#   IQR <- Q3-Q1
#   UC <- Q3+1.5*IQR
#   LC <- Q1-1.5*IQR
#   x=ifelse(x>UC, UC, x)
#   x=ifelse(x<LC, LC, x)
#   #x[x>UC1]=UC1
#   #x[x<LC1]=LC1
#   return(x)
#   
#}

out_treat <- function(x){
  UC1 = quantile(x, p=0.90,na.rm=T)
  LC1 = quantile(x, p=0.10,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
}



#-------------------------------------------------------------------------------------------------------------------------
Housing$Price = data.frame(sapply(Housing$Price, FUN=out_treat))
Housing$SqFt = data.frame(sapply(Housing$SqFt, FUN=out_treat))
Housing$Bedrooms = data.frame(sapply(Housing$Bedrooms, FUN=out_treat))

Housing_cat <- data.frame(Housing[cat_vars])
#-------------------------------------------------------------------------------------------------------------------------

cor(Housing_num)
corrplot(cor(Housing_num), method="number", number.font = 1)
summary(aov(Housing_num$Price~Housing_cat$Brick))
summary(aov(Housing_num$Price~Housing_cat$Neighborhood))


#here we are removing the bedroom beacuse it has the h
data.frame(cor(Housing_num))

Housing_cat$Brick <- ifelse(Housing_cat$Brick=="Yes",1,0)


Housing_cat$Neighborhood <- factor(Housing_cat$Neighborhood)
Dummy <- dummyVars(~Neighborhood,data=Housing_cat)
Dummy_Neighborhood <- data.frame(predict(Dummy, Housing_cat))[-1]

HousingData <- data.frame(cbind(Housing_num,Housing_cat$Brick,Dummy_Neighborhood))
names(HousingData) <- gsub(".","_", names(HousingData),fixed=T)
#-------------------------------------------------------------------------------------------------------------------------

Total_rows <- 1:nrow(HousingData)
Training_Row <- sample(Total_rows, 0.7*nrow(HousingData))
set.seed(Training_Row)
Training_data <- HousingData[Training_Row,]
Testing_data <- HousingData[-Training_Row,]
names(Training_data) <- gsub(".","_", names(Training_data),fixed=T)
names(Testing_data) <- gsub(".","_", names(Testing_data),fixed=T)
#-------------------------------------------------------------------------------------------------------------------------

summary(lm(Price~.,data = Training_data))

fit <- lm(Price~.,data = Training_data)
step(fit,direction = "both")
fit2 <- lm(formula = Price ~ SqFt + Bedrooms + Bathrooms + Offers + Housing_cat_Brick + 
             Neighborhood_West, data = Training_data)

summary(fit2)
vif(fit2)
#-------------------------------------------------------------------------------------------------------------------------
Training_data<-data.frame(cbind(Training_data, pred_Price = predict(fit2, newdata=Training_data)))
#predicting for testing data
#-------------------------------------------------------------------------------------------------------------------------
Testing_data<-data.frame(cbind(Testing_data, pred_Price = predict(fit2, newdata=Testing_data)))
#-------------------------------------------------------------------------------------------------------------------------
#Error^2(SE)
Training_data$ErrorSqr <- (Training_data$Price - Training_data$pred_Price)^2
Testing_data$ErrorSqr <- (Testing_data$Price - Testing_data$pred_Price)^2


#SSE
Train_SSE <- sum(Training_data$ErrorSqr)
Test_SSE <- sum(Testing_data$ErrorSqr)

Train_SSE
Test_SSE

#MSE
Train_MSE <- mean(Training_data$ErrorSqr)
Test_MSE <- mean(Testing_data$ErrorSqr)

Train_MSE
Test_MSE

#RMSE
Train_RMSE <- sqrt(mean((Training_data$Price - Training_data$pred_Price)^2))
Test_RMSE <- sqrt(mean((Testing_data$Price - Testing_data$pred_Price)^2))

Train_RMSE
Test_RMSE

#MAPE 
Train_MAPE <- mean(abs((Training_data$Price - Training_data$pred_Price)/Training_data$Price))
Test_MAPE <- mean(abs((Testing_data$Price - Testing_data$pred_Price)/Testing_data$Price))

Train_MAPE
Test_MAPE

#correlation
Train_cor <- cor(Training_data$Price,Training_data$pred_Price)
Test_Cor <- cor(Testing_data$Price,Testing_data$pred_Price)

Train_cor
Test_Cor


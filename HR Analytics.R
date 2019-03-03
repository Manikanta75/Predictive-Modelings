#import library which are required
#--------------------------------------------------------------------------------------------------------------------------
library(psych)
library(caret)
library(car)
library(corrplot)
library(InformationValue)
#devtools::install_github("ggplotly")
#--------------------------------------------------------------------------------------------------------------------------
#import require data into the code ground
#--------------------------------------------------------------------------------------------------------------------------
CreditData <- read.csv("C:/Users/dell/Downloads/Analytix/Micky BTCMP/R lang/Stat/BootCamp-31st August 2018/HR Analytics Case Study/HR_comma_sep.csv")
#--------------------------------------------------------------------------------------------------------------------------
str(CreditData)
head(CreditData)
describe(CreditData)

CreditData$Work_accident <- as.factor(CreditData$Work_accident)
CreditData$left <- as.factor(CreditData$left)
CreditData$promotion_last_5years <- as.factor(CreditData$promotion_last_5years)

#User defined function to create Data audit report for numerical variables
#--------------------------------------------------------------------------------------------------------------------------
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
# 
#-------------------------------------------------------------------------------------------------------------------------
Num_Data <- names(CreditData)[sapply(CreditData,is.numeric)]
Cat_Data <- names(CreditData)[sapply(CreditData,is.factor)]

Num_Data <- CreditData[,Num_Data]
Cat_Data <- CreditData[,Cat_Data]
#-------------------------------------------------------------------------------------------------------------------------
Credit_Num_Data <- data.frame(sapply(Num_Data, mystats_num))
Credit_Cat_Data <- data.frame(t(sapply(Cat_Data, mystats_cat)))
# View(Credit_Cat_Data)
#-------------------------------------------------------------------------------------------------------------------------
# Car_Num = data.frame(sapply(CarsData[,Car_Num],FUN=miss_treat_num))
boxplot(Num_Data)

#user define function for outlier treatment 
#-------------------------------------------------------------------------------------------------------------------------
out_treat <- function(x){
  UC1 = quantile(x, p=0.90,na.rm=T)
  LC1 = quantile(x, p=0.10,na.rm=T)
  x=ifelse(x>UC1, UC1, x)
  x=ifelse(x<LC1, LC1, x)
}
#outlier treatment for time_spent_company variabe because it is the only one variable that has the outliers in the data 
#-------------------------------------------------------------------------------------------------------------------------
CreditData$time_spend_company <- out_treat(CreditData$time_spend_company)
boxplot(CreditData$time_spend_company)


#creation of dummy variables
#-------------------------------------------------------------------------------------------------------------------------
Department <- (dummyVars(~department,Cat_Data,sep = "_"))
Department <- data.frame(predict(Department,Cat_Data))[-1]

Cat_Data <- cbind(Cat_Data,Department)
Cat_Data$salary <- ifelse(Cat_Data$salary=="low",0,ifelse(Cat_Data$salary=="medium",1,2))
ncol(Cat_Data)
Cat_Data <- Cat_Data[,c(1:3,5:14)]


#Combining the both numerical data and categorical variable into one data frame after the data audit  
#-------------------------------------------------------------------------------------------------------------------------

Data <- cbind(Num_Data,Cat_Data)


#here we are divinding the data into two parts which is one for trainig data set and another is for testing 
#-------------------------------------------------------------------------------------------------------------------------

Total_rows <- 1:nrow(Data)
Training_Row <- sample(Total_rows, 0.7*nrow(Data))
set.seed(Training_Row)
Training_data <- Data[Training_Row,]
Testing_data <- Data[-Training_Row,]

#Develop the model (Train the model)
#-------------------------------------------------------------------------------------------------------------------------
fit <- glm(left~.,data = Training_data,family = binomial(logit))
summary(fit)

#step wise regression we can find the optimal solution by using the step wise regression 
#IN step wise regression we will take the leat aic value which give the prefect model
#-------------------------------------------------------------------------------------------------------------------------
step(fit)

#we are removing some unnessary variables which are not significant byb keeping those elements
#-------------------------------------------------------------------------------------------------------------------------
fit2 <- glm(left ~ satisfaction_level + last_evaluation + number_project + 
              average_montly_hours + time_spend_company + Work_accident + 
              promotion_last_5years + salary + department_hr + department_management + 
              department_RandD + department_support + department_technical,
               data = Training_data,family = binomial(logit))
summary(fit2)  

vif(fit2)# Here vif value of all the variable are below 5 so we can take take the variables 

#building the model with training data set which is 70%of data 
#-------------------------------------------------------------------------------------------------------------------------
Training_data <- data.frame(cbind(Training_data,Pred_Left=predict(fit2,Training_data,type = "response")))

InformationValue::optimalCutoff(Training_data$left,Training_data$Pred_Left,optimiseFor = "Both",returnDiagnostics = TRUE)


#Training_data$Pre_New <- ifelse(Training_data$Pred_Left> 0.261,1,0)

Concordance(Training_data$left,Training_data$Pred_Left)

caret::confusionMatrix(table(Training_data$left,Training_data$Pred_Left,threshold=0.26))

Ks_Value <- ks_stat(Training_data$left,Training_data$Pred_Left,returnKSTable = T)
write.csv(Ks_Value,"Ks_values.csv")
InformationValue::plotROC(Training_data$left,Training_data$Pred_Left,Show.labels = F)
#-------------------------------------------------------------------------------------------------------------------------

Testing_data <- data.frame(cbind(Testing_data,Pred_Left=predict(fit2,Testing_data,type = "response")))

Concordance(Testing_data$left,Testing_data$Pred_Left)
caret::confusionMatrix(table(Testing_data$left,Testing_data$Pred_Left,threshold=.26))

Ks_Value <- ks_stat(Testing_data$left,Testing_data$Pred_Left,returnKSTable = T)
write.csv(Ks_Value,"Ks_values1.csv")
InformationValue::plotROC(Testing_data$left,Testing_data$Pred_Left,Show.labels = F)


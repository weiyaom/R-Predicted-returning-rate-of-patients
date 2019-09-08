#Prepare the Data Analysis
##Load packages

library(readr)
library(dplyr)
library(tidyr)
library(VIM)

# ================================================================================================

#na.strings argument is for substitution within the body of the file, that is, matching strings that should be replaced with NA
Hospital_Train<-read.csv("FileName",na.strings = c('#N/A',' ','','#VALUE!'))
Hospital_Test<-read.csv("FileName",na.strings = c('#N/A',' ','','#VALUE!'))

nrow(Hospital_Train)
nrow(Hospital_Test)

# ================================================================================================
colnames(Hospital_Train)
colnames(Hospital_Test)

Hospital_Test$RETURN = "Unknown"

dt_combined = rbind(Hospital_Train,Hospital_Test)

str(dt_combined)
View(dt_combined)

# ================================================================================================

## Type Conversion

# the columns required to be transformed into other typy: 
# (1) WEEKDAY_ARR: 'integer'->'factor'
dt_combined$WEEKDAY_ARR <- as.factor(dt_combined$WEEKDAY_ARR)
# (2) WEEKDAY_DEP: 'integer'->'factor'
dt_combined$WEEKDAY_DEP <- as.factor(dt_combined$WEEKDAY_DEP)
# (3) HOUR_ARR: 'numeric'->'factor'
dt_combined$HOUR_ARR <- as.factor(dt_combined$HOUR_ARR)
# (4) HOUR_ARR: 'numeric'->'factor'
dt_combined$HOUR_DEP <- as.factor(dt_combined$HOUR_DEP)
# (5) MONTH_ARR: 'integer'->'factor'
dt_combined$MONTH_ARR <- as.factor(dt_combined$MONTH_ARR)
# (6) MONTH_DEP: 'integer'->'factor'
dt_combined$MONTH_DEP <- as.factor(dt_combined$MONTH_DEP)
# (7) SAME_DAY: 'integer'->'factor'
dt_combined$SAME_DAY <- as.factor(dt_combined$SAME_DAY)
# (8) CONSULT_ORDER: 'integer'->'factor'
dt_combined$CONSULT_ORDER <- as.factor(dt_combined$CONSULT_ORDER)
# (9) CONSULT_CHARGE: 'integer'->'factor'
dt_combined$CONSULT_CHARGE <- as.factor(dt_combined$CONSULT_CHARGE)
# (10) CONSULT_IN_ED: 'integer'->'factor'
dt_combined$CONSULT_IN_ED <- as.factor(dt_combined$CONSULT_IN_ED)
# (11) CHARGES: 'factor'->'numeric'
dt_combined$CHARGES <- as.numeric(dt_combined$CHARGES)

# ================================================================================================

# Remove one of ArriveTime/DepartTiem: DepartTime:
dt_combined$WEEKDAY_DEP = NULL
dt_combined$HOUR_DEP = NULL
dt_combined$MONTH_DEP = NULL
dt_combined$WEEKDAY_ARR = NULL
dt_combined$HOUR_ARR = NULL
dt_combined$MONTH_ARR = NULL

# ================================================================================================

# Lasso and ridge algorithm require the input variabels to be numeric.
# matrix_dt_combined = model.matrix(~.-1-INDEX-RETURN,data=dt_combined)
matrix_dt_combined = model.matrix(~.-1-INDEX-RETURN-DC_RESULT-HOSPITAL-CONSULT_CHARGE-CONSULT_IN_ED-DIAGNOSIS-DIAG_DETAILS-SEVERITY-CHARGES,data=dt_combined)

# ================================================================================================

set.seed(1234)

dt = dt_combined[1:nrow(Hospital_Train),]
dt_pred = dt_combined[(nrow(Hospital_Train)+1):nrow(dt_combined),]
matrix_dt = matrix_dt_combined[1:nrow(Hospital_Train),]
matrix_dt_pred = matrix_dt_combined[(nrow(Hospital_Train)+1):nrow(dt_combined),]
# ================================================================================================
levels(dt$RETURN)
table(dt$RETURN)
dt$RETURN = droplevels(dt$RETURN,'Unknown')
table(dt$RETURN)
# ================================================================================================
# Split the data: train + valid + test
index_test = sample(nrow(dt),0.2*nrow(dt))
dt_test = dt[index_test,]
dt_rest = dt[-index_test,]
matrix_dt_test = matrix_dt[index_test,]
matrix_dt_rest = matrix_dt[-index_test,]

# we will use cv here, so do not need valid data ste
# index_valid = sample(nrow(dt_rest),0.2*nrow(dt_rest))
# dt_valid = dt_rest[index_valid,]
# dt_train = dt_rest[-index_valid,]

# Baseline
# table(dt_valid$RETURN)
# table(dt_valid$RETURN)[1]/sum(table(dt_valid$RETURN))
table(dt_test$RETURN)
table(dt_test$RETURN)[1]/sum(table(dt_test$RETURN))

# ================================================================================================


library(glmnet)
model_ridge_cv = cv.glmnet(x=matrix_dt_rest,y=dt_rest$RETURN,family = 'binomial',alpha=0)
model_lasso_cv = cv.glmnet(x=matrix_dt_rest,y=dt_rest$RETURN,family = 'binomial',alpha=1)
# alpha=0 (ridge)
# alpha=1 (lasso)

par(mfrow=c(1,2))
plot(model_ridge_cv)
plot(model_lasso_cv)

best_ridge_lambda = model_ridge_cv$lambda.min
best_lasso_lambda = model_lasso_cv$lambda.min

# How many variables does your best Ridge model use?
# How many variables does your best LASSO model use?
predict(model_ridge_cv,s=best_ridge_lambda,type="coefficients")
predict(model_lasso_cv,s=best_lasso_lambda,type="coefficients")

test_model_ridge_cv = predict(model_ridge_cv,s=best_ridge_lambda,newx = matrix_dt_test,type = 'response')
test_model_lasso_cv = predict(model_lasso_cv,s=best_lasso_lambda,newx = matrix_dt_test,type = 'response')

# ================================================================================================

#cutoffs = c(0.01,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95,0.99)
cutoffs = seq(0,1,0.05)
acc_test_model_ridge_cv = c()
acc_test_model_lasso_cv = c()

for (cutoff in cutoffs){
  
  class_test_model_ridge_cv = ifelse(test_model_ridge_cv>cutoff,1,0)
  table_test_model_ridge_cv = table(class_test_model_ridge_cv,dt_test$RETURN,dnn=c('Actual','Pred'))
  acc_temp = (table_test_model_ridge_cv[1]+table_test_model_ridge_cv[4])/sum(table_test_model_ridge_cv)
  acc_test_model_ridge_cv = c(acc_test_model_ridge_cv,acc_temp)
  
  class_test_model_lasso_cv = ifelse(test_model_lasso_cv>cutoff,1,0)
  table_test_model_lasso_cv = table(class_test_model_lasso_cv,dt_test$RETURN,dnn=c('Actual','Pred'))
  acc_temp = (table_test_model_lasso_cv[1]+table_test_model_lasso_cv[4])/sum(table_test_model_lasso_cv)
  acc_test_model_lasso_cv = c(acc_test_model_lasso_cv,acc_temp)
}

par(mfrow=c(1,1))
plot(x=cutoffs,y=acc_test_model_ridge_cv,xlim = c(0.2,1.0),ylim = c(0.6,0.8),type='b',main = 'Acc vs. Cutoffs',col='blue',pch=1,cex=1,ylab='Acc Rate')
lines(x=cutoffs,y=acc_test_model_lasso_cv,col='green',pch=2,type='b')
abline(h=table(dt_test$RETURN)[1]/sum(table(dt_test$RETURN)),col='red',lty=2)
legend(x=0.5,y=0.7,legend = c('Ridge','Lasso','Baseline'),col = c('blue','green','red'),lty=1,cex=0.8)

acc_logistic = data.frame(cutoffs,acc_test_model_ridge_cv,acc_test_model_lasso_cv)
acc_logistic
# ================================================================================================

pred_dt_pred = predict(model_lasso_cv,s=best_lasso_lambda,newx = matrix_dt_pred,type = 'response')
class_pred_dt_pred = ifelse(pred_dt_pred>0.4,'Yes',"No")
dt_xxx = dt_pred
dt_xxx$RETURN =class_pred_dt_pred
dt_xxx = dt_xxx[,c(1,21)]
path = paste(getwd(),"/BUDY758T_Group-1_Project_R-Code_lasso_Pred.csv",sep = "")
write_csv(dt_xxx,path)

# ================================================================================================
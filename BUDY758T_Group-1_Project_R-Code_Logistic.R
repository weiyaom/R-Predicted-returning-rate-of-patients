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

set.seed(1234)

dt = dt_combined[1:nrow(Hospital_Train),]
dt_pred = dt_combined[(nrow(Hospital_Train)+1):nrow(dt_combined),]
# ================================================================================================

# Split the data: train + valid + test
index_test = sample(nrow(dt),0.2*nrow(dt))
dt_test = dt[index_test,]
dt_rest = dt[-index_test,]

index_valid = sample(nrow(dt_rest),0.2*nrow(dt_rest))
dt_valid = dt_rest[index_valid,]
dt_train = dt_rest[-index_valid,]

# Baseline
table(dt_valid$RETURN)
table(dt_valid$RETURN)[1]/sum(table(dt_valid$RETURN))
table(dt_test$RETURN)
table(dt_test$RETURN)[1]/sum(table(dt_test$RETURN))

# Logistic Regression:
# Since in this project, we are trying to predict if a patient will return to the hospital within one(1)month or not.
# It is a binaory prediction (a classsification problem)
# We would consider building a Logistic Regression since it is simple and we could get the details(likelyhood) of the prediction result
# We are going to build multiple models with diffierent variable combinations:
# (1) Full-Model: use all the variables in the data set as the input variables. It is a naive approach, but we are going to tuning the parameters and try to improve our prediction performance.
# (2) Human-Selected-Model(commen sense/): without any calculation, we could use our commen sense: what are the factors affecting the paitent status?
# (3) Forward-Selected-Model: instead of using human experience to make selection, we are going to use forward selection methodology
# We are going to compare the prediction result of these 4 models

# (1) Full-Model
# note: since Arrive time and Depart time variables have very high correlation,
# we are only going to use Arrive time (Depart Time Already Deleted)
# (Note: omitted variable (DC-RESULT), since this variable has too many levels, and some level only contain one or two observation, which will lead to error after data partitioning.)
model_logistic_full = glm(RETURN ~ . -INDEX-DC_RESULT, family='binomial',data = dt_train)
model_logistic_full
summary(model_logistic_full)

colnames(dt_train)

# (2) Human-Selected
model_logistic_human = glm(RETURN~AGE+FINANCIAL_CLASS+ED_RESULT+ACUITY_ARR+ADMIT_RESULT
                           +CONSULT_ORDER+CONSULT_IN_ED+DIAGNOSIS+DIAG_DETAILS+RISK+SEVERITY+CHARGES,
                           family = 'binomial',data = dt_train)
model_logistic_human
summary(model_logistic_human)

library('MASS')
#my_step = stepAIC(model_logistic)
#step(model_logistic)
model_logistic_null = glm(RETURN~1,data = dt_train,family = 'binomial')

# (3) forward-selected
step(model_logistic_null,scope = list(upper=model_logistic_full),direction = 'forward')


# Call:  glm(formula = RETURN ~ ED_RESULT + FINANCIAL_CLASS + GENDER + 
#              ETHNICITY + RACE + CHARGES + ACUITY_ARR + CONSULT_ORDER + 
             # AGE + ADMIT_RESULT + RISK + SAME_DAY, family = "binomial", 
           # data = dt_train)

model_logistic_opt = glm(formula = RETURN ~ ED_RESULT + FINANCIAL_CLASS + GENDER + 
                                  ETHNICITY + RACE + CHARGES + ACUITY_ARR + CONSULT_ORDER + 
                                  AGE + ADMIT_RESULT + RISK + SAME_DAY, family = "binomial", 
                                data = dt_train)

summary(model_logistic_opt)

# now we have 3 models:
# model_logistic_full
# model_logistic_human
# model_logistic_opt

#cutoffs = c(0.01,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,0.95,0.99)
cutoffs = seq(0,1,0.05)
acc_logistic_full = c()
acc_logistic_human = c()
acc_logistic_opt = c()

for (cutoff in cutoffs){
  
  pred_logistic_valid_full = predict(model_logistic_full,dt_valid,type = 'response')
  class_logistic_valid_full = ifelse(pred_logistic_valid_full>cutoff,1,0)
  table_logistic_valid_full = table(class_logistic_valid_full,dt_valid$RETURN,dnn=c('Actual','Pred'))
  acc_temp = (table_logistic_valid_full[1]+table_logistic_valid_full[4])/sum(table_logistic_valid_full)
  acc_logistic_full = c(acc_logistic_full,acc_temp)
  
  pred_logistic_valid_human = predict(model_logistic_human,dt_valid,type = 'response')
  class_logistic_valid_human = ifelse(pred_logistic_valid_human>cutoff,1,0)
  table_logistic_valid_human = table(class_logistic_valid_human,dt_valid$RETURN,dnn=c('Actual','Pred'))
  acc_temp = (table_logistic_valid_human[1]+table_logistic_valid_human[4])/sum(table_logistic_valid_human)
  acc_logistic_human = c(acc_logistic_human,acc_temp)
  
  pred_logistic_valid_opt = predict(model_logistic_opt,dt_valid,type = 'response')
  class_logistic_valid_opt = ifelse(pred_logistic_valid_opt>cutoff,1,0)
  table_logistic_valid_opt = table(class_logistic_valid_opt,dt_valid$RETURN,dnn=c('Actual','Pred'))
  acc_temp = (table_logistic_valid_opt[1]+table_logistic_valid_opt[4])/sum(table_logistic_valid_opt)
  acc_logistic_opt = c(acc_logistic_opt,acc_temp)
}

plot(x=cutoffs,y=acc_logistic_full,xlim = c(0.2,1.0),ylim = c(0.6,0.8),type='b',main = 'Logistic Acc vs. Cutoffs',col='blue',pch=1,cex=1,ylab='Acc Rate')
lines(x=cutoffs,y=acc_logistic_human,col='green',pch=2,type='b')
lines(x=cutoffs,y=acc_logistic_opt,col='red',pch=3,type='b')
abline(h=table(dt_valid$RETURN)[1]/sum(table(dt_valid$RETURN)),col='black',lty=2)
legend(x=0.5,y=0.7,legend = c('Full','Human-Selected','Fwd-Select','Baseline'),col = c('blue','green','red','black'),lty=1,cex=0.8)

acc_logistic = data.frame(cutoffs,acc_logistic_full,acc_logistic_human,acc_logistic_opt)
acc_logistic
# As we could observed fromt the plot, When we choose cutoff = 0.5, our acc could be slightly better than the baseline
# unfortunately, the acc rate did not show much variation among four diffierent models.
# I would suggest to use the variables selected bt forward-selectiion method,
# which has the fewest number of variables and lowest AIC
# To further improve our prediction acc rate, we may need to consider building other models and compare the result afterwords

model_logistic_opt_retrain = glm(formula = RETURN ~ ED_RESULT + FINANCIAL_CLASS + GENDER + 
                                   ETHNICITY + RACE + ACUITY_ARR + CONSULT_ORDER + RISK + ADMIT_RESULT + 
                                   AGE + SAME_DAY, family = "binomial", data = dt_rest)
summary(model_logistic_opt_retrain)

pred_logistic_test_opt = predict(model_logistic_opt_retrain,dt_test,type = 'response')
class_logistic_test_opt = ifelse(pred_logistic_test_opt>0.4,1,0)
table_logistic_test_opt = table(class_logistic_test_opt,dt_test$RETURN,dnn=c('Actual','Pred'))
acc_logistic_test = (table_logistic_test_opt[1]+table_logistic_test_opt[4])/sum(table_logistic_test_opt)
acc_logistic_test


# ================================================================================================

pred_dt_pred = predict(model_logistic_opt_retrain,dt_pred,type = 'response')
class_pred_dt_pred = ifelse(pred_dt_pred>0.4,'Yes',"No")
dt_xxx = dt_pred
dt_xxx$RETURN =class_pred_dt_pred
dt_xxx = dt_xxx[,c(1,21)]
path = paste(getwd(),"/BUDY758T_Group-1_Project_R-Code_Logistic_Pred.csv",sep = "")
write_csv(dt_xxx,path)

# ================================================================================================
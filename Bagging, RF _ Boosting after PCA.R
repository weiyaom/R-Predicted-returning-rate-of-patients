library(readr)
#na.strings argument is for substitution within the body of the file, that is, matching strings that should be replaced with NA
Hospital_Train<-read.csv("5.3_Hospitals_train_cleaned.csv",na.strings = c('#N/A',' ','','#VALUE!'))
Hospital_Test<-read.csv("5.3_Hospitals_test_cleaned.csv",na.strings = c('#N/A',' ','','#VALUE!'))

nrow(Hospital_Train)
nrow(Hospital_Test)

colnames(Hospital_Train)
colnames(Hospital_Test)

Hospital_Test$RETURN = "Unknown"

dt_combined = rbind(Hospital_Train,Hospital_Test)

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

# Remove one of ArriveTime/DepartTiem: DepartTime:
dt_combined$WEEKDAY_DEP = NULL
dt_combined$HOUR_DEP = NULL
dt_combined$MONTH_DEP = NULL
dt_combined$WEEKDAY_ARR = NULL
dt_combined$HOUR_ARR = NULL
dt_combined$MONTH_ARR = NULL
dt_combined$RETURN = as.factor(ifelse(dt_combined$RETURN=='Yes',1,0))

# combination 1
# Ed_result, Charges, Financial class, Age, Gender, Acuity_arr, Dc_result, PC (all others)
X <- model.matrix( ~ .-1, dt_combined[,c(2,5,6,8,12:19)])

# combination 2
# PC all variables that step() not choose
# X <- model.matrix( ~ .-1, dt_combined[,c(2,11,14:17,19,20)])

# combination 3
#Ed_result, Charges, Financial class, Age, Gender, Acuity_arr, PC (all others)
# X <- model.matrix( ~ .-1, dt_combined[,c(2,5,6,8,11:19)])

PC_X = prcomp(X)
summary(PC_X)
plot(PC_X)
PC1 = PC_X$x[,1]
PC2 = PC_X$x[,2]
dt_combined$PC = PC1
dt_combined$PC2 = PC2
#-----------------------
set.seed(1234)

dt = dt_combined[1:nrow(Hospital_Train),]
dt_pred = dt_combined[(nrow(Hospital_Train)+1):nrow(dt_combined),]

# Split the data: train + valid + test
index_test = sample(nrow(dt),0.2*nrow(dt))
dt_test = dt[index_test,]
dt_rest = dt[-index_test,]

# combination 1
################################################################################################

library(randomForest)
# 200 trees: 
rf.200 = randomForest(RETURN ~ ED_RESULT + CHARGES + FINANCIAL_CLASS +  AGE +
                        GENDER + ACUITY_ARR  + DC_RESULT + PC,
                      data=dt_rest, ntree=200, mytry=5,importance=TRUE)
importance(rf.200)
varImpPlot(rf.200)

rf_200_pre = predict(rf.200,newdata = dt_test)
acc_rf_200 = sum(ifelse(rf_200_pre==dt_test$RETURN,1,0))/nrow(dt_test)
acc_rf_200 
# Ed_result, Charges, Financial class, Age, Gender, Acuity_arr, Dc_result, PC (all others)

library(gbm)
boost_rest=gbm(as.numeric(RETURN)-1 ~ ED_RESULT + CHARGES + FINANCIAL_CLASS +  AGE +
                 GENDER + ACUITY_ARR  + DC_RESULT + PC, 
               data = dt_rest, n.trees=200, distribution="bernoulli")
summary(boost_rest)

boost_preds=predict(boost_rest,newdata=dt_test,n.trees=200,type="response")
boost_preds_class = ifelse(boost_preds>0.5,'1',"0")
table_preds_pca = table(boost_preds_class,dt_test$RETURN,dnn=c('Actual','Pred'))
acc_preds = (table_preds_pca[1]+table_preds_pca[4])/sum(table_preds_pca)
acc_preds

# combination 2
##################################################################################################

library(randomForest) # 200 trees: 
rf.200 = randomForest(RETURN ~ ED_RESULT + FINANCIAL_CLASS + GENDER + ETHNICITY + RACE + 
                        ACUITY_ARR + CONSULT_ORDER + RISK + ADMIT_RESULT + AGE + SAME_DAY + PC,
                      data=dt_rest, ntree=200, mytry=5,importance=TRUE)
importance(rf.200)
varImpPlot(rf.200)

rf_200_pre = predict(rf.200,newdata = dt_test)
acc_rf_200 = sum(ifelse(rf_200_pre==dt_test$RETURN,1,0))/nrow(dt_test)
acc_rf_200 

library(gbm)
boost_rest=gbm(as.numeric(RETURN)-1 ~ ED_RESULT + FINANCIAL_CLASS + GENDER + ETHNICITY + RACE + 
                 ACUITY_ARR + CONSULT_ORDER + RISK + ADMIT_RESULT + AGE + SAME_DAY + PC, 
                data = dt_rest, n.trees=200, distribution="bernoulli")
summary(boost_rest)

boost_preds=predict(boost_rest,newdata=dt_test,n.trees=200,type="response")
boost_preds_class = ifelse(boost_preds>0.5,'1',"0")
table_preds_pca = table(boost_preds_class,dt_test$RETURN,dnn=c('Actual','Pred'))
acc_preds = (table_preds_pca[1]+table_preds_pca[4])/sum(table_preds_pca)
acc_preds

library(randomForest)
bag.200=randomForest(RETURN ~ ED_RESULT + FINANCIAL_CLASS + GENDER + ETHNICITY + RACE + 
                       ACUITY_ARR + CONSULT_ORDER + RISK + ADMIT_RESULT + AGE + SAME_DAY + PC,
                     data=dt_rest,ntree=200,mtry=12,importance=TRUE)
bag.200
importance(bag.200)
varImpPlot(bag.200)

bag_200_pre = predict(bag.200,newdata = dt_test)
acc_bag_200 = sum(ifelse(bag_200_pre==dt_test$RETURN,1,0))/nrow(dt_test)
acc_bag_200

# combination 3
##################################################################################################
library(randomForest)
rf.200 = randomForest(RETURN ~ ED_RESULT + CHARGES + FINANCIAL_CLASS +  AGE +
                        GENDER + ACUITY_ARR + PC,
                      data=dt_rest, ntree=200, mytry=5,importance=TRUE)
importance(rf.200)
varImpPlot(rf.200)

rf_200_pre = predict(rf.200,newdata = dt_test)
acc_rf_200 = sum(ifelse(rf_200_pre==dt_test$RETURN,1,0))/nrow(dt_test)
acc_rf_200

library(gbm)
boost_rest=gbm(as.numeric(RETURN)-1 ~ ED_RESULT + CHARGES + FINANCIAL_CLASS +  AGE +
                 GENDER + ACUITY_ARR + PC, data = dt_rest, n.trees=200, distribution="bernoulli")
summary(boost_rest)

boost_preds=predict(boost_rest,newdata=dt_test,n.trees=200,type="response")
boost_preds_class = ifelse(boost_preds>0.5,'1',"0")
table_preds_pca = table(boost_preds_class,dt_test$RETURN,dnn=c('Actual','Pred'))
acc_preds = (table_preds_pca[1]+table_preds_pca[4])/sum(table_preds_pca)
acc_preds

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Test_pred = predict(boost_rest,newdata = dt_pred,n.trees=200,type="response")
#result = ifelse(Test_pred>0.5,'YES',"NO")
#write.csv(result,'5.12 PCB Final.csv')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Test_prediction
#Test_pred = predict(rf.200,newdata = dt_pred)
#result = ifelse(Test_pred=='1','Yes','No')
#write.csv(result,'5.12 RFPC.csv')
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
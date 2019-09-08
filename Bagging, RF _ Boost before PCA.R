# 758T Project--Bagging, Random Forest & Boosting before PCA

train <- read.csv("5.3_Hospitals_train_cleaned.csv",na.strings = c('#N/A',' ','','#VALUE!'))
Test <- read.csv("5.3_Hospitals_test_cleaned.csv",na.strings = c('#N/A',' ','','#VALUE!'))

Test$RETURN = "Unknown"

dt_combined = rbind(train,Test)

str(dt_combined)
View(dt_combined)

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

dt_combined$WEEKDAY_DEP = NULL
dt_combined$HOUR_DEP = NULL
dt_combined$MONTH_DEP = NULL
dt_combined$WEEKDAY_ARR = NULL
dt_combined$HOUR_ARR = NULL
dt_combined$MONTH_ARR = NULL

dt_combined$RETURN = as.factor(ifelse(dt_combined$RETURN=='Yes',1,0))

set.seed(1234)

dt = dt_combined[1:nrow(train),]
dt_test = dt_combined[(nrow(train)+1):nrow(dt_combined),]


# Split the data: rest + test
num_obs=nrow(dt)
test_obs = sample(num_obs, 0.2*num_obs)
test <- dt[test_obs,]

# Save the rest of the data
rest <- dt[-test_obs,]

## Bagging
# 200 trees:
library(randomForest)

bagging.300=randomForest(RETURN~.-INDEX,data=rest,ntree=300,mtry=19,importance=TRUE)
bagging.300
importance(bagging.300)
varImpPlot(bagging.300)

bagging_300_pre = predict(bagging.300,newdata = test)
acc_bagging_300 = sum(ifelse(bagging_300_pre==test$RETURN,1,0))/nrow(test)
acc_bagging_300

## Random Forest
# 200 trees:
rf.200 = randomForest(RETURN~.-INDEX,data=rest,ntree=200,mytry=4,importance=TRUE)
rf.200
importance(rf.200)
varImpPlot(rf.200)

rf_200_pre = predict(rf.200,newdata = test)
acc_rf_200 = sum(ifelse(rf_200_pre==test$RETURN,1,0))/nrow(test)
acc_rf_200

# 500 trees:
rf.500 = randomForest(RETURN~.-INDEX,data=rest,ntree=500,mytry=4,importance=TRUE)
rf.500
importance(rf.500)
varImpPlot(rf.500)

rf_500_pre = predict(rf.500,newdata = test)
acc_rf_500 = sum(ifelse(rf_500_pre==test$RETURN,1,0))/nrow(test)
acc_rf_500

# 1000 trees: 
rf.1000 = randomForest(RETURN~.-INDEX,data=rest,ntree=1000,mytry=4,importance=TRUE)
rf.1000
importance(rf.1000)
varImpPlot(rf.1000)

rf_1000_pre = predict(rf.1000,newdata = test)
acc_rf_1000 = sum(ifelse(rf_1000_pre==test$RETURN,1,0))/nrow(test)
acc_rf_1000


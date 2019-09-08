Hospital_Train<-read.csv("Downloads/5.3Hospitals_train_cleaned.csv",na.strings = c('#N/A',' ','','#VALUE!'))

Hospital_Train$WEEKDAY_DEP = NULL
Hospital_Train$HOUR_DEP = NULL
Hospital_Train$MONTH_DEP = NULL
Hospital_Train$WEEKDAY_ARR = NULL
Hospital_Train$HOUR_ARR = NULL
Hospital_Train$MONTH_ARR = NULL

Hospital_Test$MONTH_ARR = NULL

str(Hospital_Train)

Hospital_Train_matrix = model.matrix(~.-1,data=Hospital_Train[,-c(1)])

df_Train = data.frame(Hospital_Train_matrix)
names(df_Train)

#Rename "RETURNYes"  as RETURN
names(df_Train)[105]<-"RETURN"
names(df_Train)
set.seed(1234)

## First, partition 35% of the data for testing data
num_obs=nrow(df_Train)
test_obs = sample(num_obs, 0.2*num_obs)
test <- df_Train[test_obs,]

## Save the rest of the data as the data that isn't testing
rest <- df_Train[-test_obs,]

library(gbm)
boost_model = gbm(RETURN~.,data=rest,n.trees=300,distribution="bernoulli")
summary(boost_model)

preds_train <- predict(object = boost_model, 
                 newdata = rest,
                 n.trees = 300,
                 type = "response")

train_boosting_class <- ifelse(preds_train>0.5,1,0)
sum(ifelse(train_boosting_class==rest$RETURN,1,0))/nrow(rest)

preds <- predict(object = boost_model, 
                  newdata = test,
                  n.trees = 300,
                  type = "response")

boosting_class <- ifelse(preds>0.5,1,0)
table(test$RETURN,boosting_class)
sum(ifelse(boosting_class==test$RETURN,1,0))/nrow(test)




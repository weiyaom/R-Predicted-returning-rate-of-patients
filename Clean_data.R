library(plyr)
library(mice)
library(dplyr)
library(tidyr)
library(VIM)
Hospitals_Train<-read.csv("Hospitals_Train.csv",na.strings = c('#N/A',' ','','#VALUE!'))
Hospitals_Test<-read.csv("Hospitals_Test_X.csv",na.strings = c('#N/A',' ','','#VALUE!'))

sum(is.na(Hospitals_Train$INDEX))
Hospitals_Train<-filter(Hospitals_Train, Hospitals_Train$INDEX <= 38221)
dim(Hospitals_Train)

Hospitals_Test$RETURN = "Unknown"

#RETURN:this is our targeting variable, we drop those 141 rows to avoid to affect modeling and prediction.
sum(is.na(Hospitals_Train$RETURN ))
Hospitals_Train  <- Hospitals_Train  %>% drop_na(RETURN)

combined = rbind(Hospitals_Train,Hospitals_Test)

str(combined)

combined$WEEKDAY_ARR <- as.factor(combined$WEEKDAY_ARR)
# (2) WEEKDAY_DEP: 'integer'->'factor'
combined$WEEKDAY_DEP <- as.factor(combined$WEEKDAY_DEP)
# (3) HOUR_ARR: 'numeric'->'factor'
combined$HOUR_ARR <- as.factor(combined$HOUR_ARR)
# (4) HOUR_ARR: 'numeric'->'factor'
combined$HOUR_DEP <- as.factor(combined$HOUR_DEP)
# (5) MONTH_ARR: 'integer'->'factor'
combined$MONTH_ARR <- as.factor(combined$MONTH_ARR)
# (6) MONTH_DEP: 'integer'->'factor'
combined$MONTH_DEP <- as.factor(combined$MONTH_DEP)
# (7) SAME_DAY: 'integer'->'factor'
combined$SAME_DAY <- as.factor(combined$SAME_DAY)
# (8) CONSULT_ORDER: 'integer'->'factor'
combined$CONSULT_ORDER <- as.factor(combined$CONSULT_ORDER)
# (9) CONSULT_CHARGE: 'integer'->'factor'
combined$CONSULT_CHARGE <- as.factor(combined$CONSULT_CHARGE)
# (10) CONSULT_IN_ED: 'integer'->'factor'
combined$CONSULT_IN_ED <- as.factor(combined$CONSULT_IN_ED)
# (11) CHARGES: 'factor'->'numeric'
combined$CHARGES <- as.numeric(combined$CHARGES)

names(combined)
dupe = combined[,-c(1,27)]
combined[duplicated(dupe) | duplicated(dupe, fromLast=TRUE),]

colnames(combined)[colSums(is.na(combined)) > 0]

#(1) GENDER we fill with 'Male' (majority)
table(combined$GENDER)
combined$GENDER[is.na(combined$GENDER)] <- 'Male'
sum(is.na(combined$GENDER))

#(2)RACE: 
table(combined$RACE)
sum(is.na(combined$RACE))
combined$RACE[is.na(combined$RACE)] <- 'Unknown'

#(3)ETHNICITY:
table(combined$ETHNICITY)
sum(is.na(combined$ETHNICITY))
combined$ETHNICITY[is.na(combined$ETHNICITY)] <- 'Unknown'

#(4)ED_RESULT: 88 missing values let's fill with major value (Discharge)
table(combined$ED_RESULT)
sum(is.na(combined$ED_RESULT))
levels(Hospitals_Test$ED_RESULT)
levels(Hospitals_Train$ED_RESULT)
combined$ED_RESULT[is.na(combined$ED_RESULT)] <- 'Discharge'

#(5)ACUITY_ARR 4279
sum(is.na(combined$ACUITY_ARR))
levels(combined$ACUITY_ARR) <- c(levels(combined$ACUITY_ARR), "Unknown")
combined$ACUITY_ARR[is.na(combined$ACUITY_ARR)] <- 'Unknown'

levels(combined$ACUITY_ARR) 
# * for '5 Purple', we think it is a typo and we replace this value with '5-Non-Urgent'
combined$ACUITY_ARR[combined$ACUITY_ARR=='5 Purple'] <- '5-Non-Urgent'
combined$ACUITY_ARR=factor(combined$ACUITY_ARR)

#(6)DC_RESULT
sum(is.na(combined$DC_RESULT))
combined$DC_RESULT <- as.factor(combined$DC_RESULT) 
levels(combined$DC_RESULT)
levels(combined$DC_RESULT) <- list('Home or Self Care'=c("Home Health Care Svc", "Home or Self Care","Hospice/Home"), 
                                          'Not specified Other or Unknown'=c('Still a Patient','Expired',
                                                                             'Not specified Other or Unknown','AWOL - IP Only',
                                                                             'Left Against Medical Advice','Left Prior to Exam',
                                                                             'LEFT W/O BEING SEEN AFTER TRIAGE',
                                                                             'LEFT W/O BEING SEEN BEFORE TRIAGE','LEFT W/O COMPLETED TREATMENT'))
combined$DC_RESULT <- as.character(Hospitals_train$DC_RESULT)
combined$DC_RESULT[is.na(Hospitals_train$DC_RESULT)] <- 'Discharged to other facility'
combined$DC_RESULT <- as.factor(Hospitals_train$DC_RESULT)
summary(combined$DC_RESULT)
 
levels(combined$DC_RESULT)
summary(combined$DC_RESULT)

#（7）ADMIT_RESULT
table(combined$ADMIT_RESULT)
sum(is.na(combined$ADMIT_RESULT))
levels(combined$ADMIT_RESULT)
levels(combined$ADMIT_RESULT) <- c(levels(combined$ADMIT_RESULT), "Unknown")
combined$ADMIT_RESULT[is.na(combined$ADMIT_RESULT)] <- 'Unknown'

#(8）RISK：43324 NA
table(combined$RISK)
sum(is.na(combined$RISK))
levels(combined$RISK) <- c(levels(combined$RISK), "Unknown")
combined$RISK[is.na(combined$RISK)] <- 'Unknown'
levels(combined$RISK)
str(combined$RISK)

# (9)SEVERITY: 43324 NA
table(combined$SEVERITY)
sum(is.na(combined$SEVERITY))
levels(combined$SEVERITY) <- c(levels(combined$SEVERITY), "Unknown")
combined$SEVERITY[is.na(combined$SEVERITY)] <- 'Unknown'
levels(combined$SEVERITY)

#(10)CONSULT_IN_ED
sum(is.na(combined$CONSULT_IN_ED))
table(combined$CONSULT_IN_ED)
levels(combined$CONSULT_IN_ED) <- c(levels(combined$CONSULT_IN_ED), '0')
combined$CONSULT_IN_ED[is.na(combined$CONSULT_IN_ED)] <- '0'

#(11)CHARGES
sum(is.na(combined$CHARGES))
mice_imputes = mice(combined, m = 5, maxit = 5,seed=1234)
summary(mice_imputes)
combined <- mice::complete(mice_imputes,5)

summary(combined)

colnames(combined)[colSums(is.na(combined)) > 0]
Train = combined[1:nrow(Hospitals_Train),]
Test_X = combined[(nrow(Hospitals_Train)+1):nrow(combined),c(1:26)]
names(Test_X)

write.csv(Train , file = "Hospitals_train_cleaned.csv",row.names = FALSE)
write.csv(Test_X, file = "Hospitals_test_cleaned.csv",row.names = FALSE)

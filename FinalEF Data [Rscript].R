#Install packages
install.packages ("Hmisc")
install.packages ("dplyr")

library("psych")
library("Hmisc")
library ("dplyr")
library("")

#set working directory
getwd()
setwd("/Users/Shannan/Downloads/Culture, Emotion & Health Lab/R Studio")
setwd("/Users/Shannan/Downloads/Culture, Emotion & Health Lab/")

#import data set
EFdata <-read.csv("EF & Mindfulness.csv", header =TRUE)
FinalEFdata <-read.csv("FinalEFdata.csv", header =TRUE)

#descriptive data
names(EFdata)
head(EFdata)

#remove the participants with 3 or more attention checks
filter(EFdata, failed >= 3)

#REVERSE CODE [FOR THE MINDFULNESS QUESTIONNAIRE]
#step 1: make another copy of the EF data & put it in another folder (this folder's name is "scores")
scores <- filter(EFdata, failed >= 3)

#step 2: labeling the columns of the data that we want to reverse code
columnsToReverse <- c('ffmq5','ffmq8','ffmq18','ffmq23','ffmq28','ffmq34','ffmq38')

#step 3:look at the original data for those columns (making sure that the data correctly appears in a matrix box)
scores[,columnsToReverse]

#step 4:type the formula to reverse code (6 minus the original number= the reverse coded number)
  6-scores[,columnsToReverse]

#step 5: make a copy of the original data & put it in a new folder [the name of this folder is "reversedEFData"]
reversedEFdata <- EFdata
  
#step 6: reverse code the copied version of the data
6-reversedEFdata[,columnsToReverse]
  
#step 7: put the altered columns back into the "reversedEFdata" folder]
reversedEFdata[,columnsToReverse] <- 6-reversedEFdata[,columnsToReverse]

#step 8: look at the reversedEFdata folder (making sure that the altered data correctly appears)
reversedEFdata

#CALCULATE THE "ACTING WITH AWARENESS" SUBSCALE SCORE [FOR THE MINDFULNESS QUESTIONNAIRE]
#step 1: label the columns of the data that we want to average
ActingwithAwareness_score <- c('ffmq5','ffmq8','ffmq13','ffmq18','ffmq23','ffmq28','ffmq38')

#step 2: calculate the means for the relevant columns (for each participant) [trial run]
rowMeans(reversedEFdata[,ActingwithAwareness_score])

#step 3:name the column name for the calculated means
rowAvgColumnName <- c("ActingwithAwareness_score")

#step 4: add the calculated mean values to the end of the original data [put it in the "reversedEFdata folder"]
reversedEFdata[rowAvgColumnName] <- rowMeans(reversedEFdata[,ActingwithAwareness_score])

#CALCULATE THE "NON-REACTIVITY TO INNER EXPERIENCE" SUBSCALE SCORE [FOR THE MINDFULNESS QUESTIONNAIRE]
#step 1: label the columns of the data that we want to average
NonReactivitytoInnerExperience_score <- c('ffmq4','ffmq9','ffmq19','ffmq21','ffmq24','ffmq29','ffmq33')

#step 2: calculate the means for the relevant columns (for each participant) [trial run]
rowMeans(reversedEFdata[,NonReactivitytoInnerExperience_score])

#step 3: name the column name for the calculated means
rowAvgColumnName <- c("NonReactivitytoInnerExperience_score")

#step 4: add the calculated mean values to the end of the original data [put it in the "reversedEFdata folder"]
reversedEFdata[rowAvgColumnName] <- rowMeans(reversedEFdata[,NonReactivitytoInnerExperience_score])

#step 5:rename the dataframe
finalEFdata <-reversedEFdata

#CALCULATE THE "NEGATIVE SUPPRESSION" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('free13','free14','free15','free16')
ColumnNameofAvg <- c('NegativeSuppression_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "POSITIVE SUPPRESSION" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('free9','free10','free11','free12')
ColumnNameofAvg <- c('PositiveSuppression_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "NEGATIVE ENHANCEMENT" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('free5','free6','free7','free8')
ColumnNameofAvg <- c('NegativeEnhancement_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "POSITIVE ENHANCEMENT" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('free1','free2','free3','free4')
ColumnNameofAvg <- c('PositiveEnhancement_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "OVERALL SUPPRESSION" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('NegativeSuppression_score','PositiveSuppression_score')
ColumnNameofAvg <- c('OverallSuppression_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "OVERALL ENHANCEMENT" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
AvgofColumn <- c('NegativeEnhancement_score','PositiveEnhancement_score')
ColumnNameofAvg <- c('OverallEnhancement_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#CALCULATE THE "POLARITY" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
ColumnNameofAvg <- c('Polarity_score')
finalEFdata[,ColumnNameofAvg] <-abs(finalEFdata["OverallSuppression_score"]-finalEFdata["OverallEnhancement_score"])

#CALCULATE THE "OVERALL FLEXIBILITY" SUBSCALE SCORE [FOR THE EF QUESTIONNAIRE]
ColumnNameofAvg <- c('OverallFlexibility_score')
finalEFdata[,ColumnNameofAvg] <-finalEFdata["OverallSuppression_score"] +finalEFdata["OverallEnhancement_score"]- finalEFdata["Polarity_score"]

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL FLEXIBILITY & ACTING WITH AWARENESS
cor.test(finalEFdata$OverallFlexibility_score,finalEFdata$ActingwithAwareness_score, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL FLEXIBILITY & NON-REACTIVITY TO INNER EXPERIENCE
cor.test(finalEFdata$OverallFlexibility_score,finalEFdata$NonReactivitytoInnerExperience_score, method = c("pearson"))

#REVERSE CODE [FOR THE CSI QUESTIONNAIRE]
#step 1: labeling the columns of the data that we want to reverse code
ColumnToReverse <- c('csi1b','csi1c','csi1d','csi2a','csi4a','csi4b','csi4c','csi5a','csi5b','csi5c')

#step 2: put the altered columns back into the "finalEFdata" folder]
finalEFdata[,columnsToReverse] <- 8-finalEFdata[,columnsToReverse]

#CALCULATE THE "CUE PRESENCE" SUBSCALE SCORE [FOR THE CSI QUESTIONNAIRE]
ColumnNameofAvg <- c('CuePresence_score')
ColumnToSums <- c('csi1a','csi2b','csi2c','csi2d','csi3a','csi3b','csi3c','csi6a','csi6b','csi6c')
finalEFdata[,ColumnNameofAvg] <- rowSums(finalEFdata[ColumnToSums])

#CALCULATE THE "CUE Absence" SUBSCALE SCORE [FOR THE CSI QUESTIONNAIRE]
ColumnNameofAvg <- c('CueAbsence_score')
ColumnToSums <- c('csi1b','csi1c','csi1d','csi2a','csi4a','csi4b','csi4c','csi5a','csi5b','csi5c')
finalEFdata[,ColumnNameofAvg] <- rowSums(finalEFdata[ColumnToSums])

#CALCULATE THE "OVERALL CONTEXT SENSITIVITY" SUBSCALE SCORE [FOR THE CSI QUESTIONNAIRE]
AvgofColumn <- c('CuePresence_score','CueAbsence_score')
ColumnNameofAvg <- c('OverallContextSensitivity_score')
finalEFdata[,ColumnNameofAvg] <-rowMeans (finalEFdata[,AvgofColumn])

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL CONTEXT SENSITIVITY & ACTING WITH AWARENESS
cor.test(finalEFdata$OverallContextSensitivity_score,finalEFdata$ActingwithAwareness_score, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL CONTEXT SENSITIVITY & NON-REACTIVITY TO INNER EXPERIENCE
cor.test(finalEFdata$OverallContextSensitivity_score,finalEFdata$NonReactivitytoInnerExperience_score, method = c("pearson"))

#OUTPUT OF THE FINAL EF DATA
write.csv(finalEFdata,"FinalEFdata.csv", row.names = TRUE)

#EF & DEMOGRAPHIC VARIABLES [PART 2]
#set working directory
getwd()
setwd("/Users/Shannan/Downloads/Culture, Emotion & Health Lab/EF & Mindfulness/R Studio")

#import data set
FinalEFdata <-read.csv("FinalEFdata.csv", header =TRUE)

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL FLEXIBILITY & GENDER
cor.test(FinalEFdata$OverallFlexibility_score,FinalEFdata$gender, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN OVERALL FLEXIBILITY & AGE
cor.test(FinalEFdata$OverallFlexibility_score,FinalEFdata$age, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN NEGATIVE SUPPRESSION & ACTING WITH AWARENESS
cor.test(FinalEFdata$NegativeSuppression_score,FinalEFdata$ActingwithAwareness_score, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN POSITIVE SUPPRESSION & ACTING WITH AWARENESS
cor.test(FinalEFdata$PositiveSuppression_score,FinalEFdata$ActingwithAwareness_score, method = c("pearson"))

#RUN A SIMPLE, PAIRWISE CORRELATION BETWEEN POSITIVE ENHANCEMENT & ACTING WITH AWARENESS
cor.test(FinalEFdata$PositiveEnhancement_score,FinalEFdata$ActingwithAwareness_score, method = c("pearson"))

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER OVERALL FLEXIBILITY DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$OverallFlexibility_score ~FinalEFdata$gender, data= FinalEFdata)
result

#TO DETERMINE THE HIGHEST & LOWEST VALUE FOR THE OVERALL FLEXIBILITY SCORE
max(FinalEFdata$OverallFlexibility_score, na.rm = TRUE)
min(FinalEFdata$OverallFlexibility_score, na.rm = TRUE)

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER NEGATIVE SUPPRESSION DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$NegativeSuppression_score ~FinalEFdata$gender, data= FinalEFdata)
result

#TO DETERMINE THE HIGHEST & LOWEST VALUE FOR THE NEGATIVE SUPPRESSION SCORE
max(FinalEFdata$NegativeSuppression_score, na.rm = TRUE)
min(FinalEFdata$NegativeSuppression_score, na.rm = TRUE)

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER POSITIVE SUPPRESSION DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$PositiveSuppression_score ~FinalEFdata$gender, data= FinalEFdata)
result

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER POSITIVE ENHANCEMENT DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$PositiveEnhancement_score ~FinalEFdata$gender, data= FinalEFdata)
result

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER NEGATIVE ENHANCEMENT DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$NegativeEnhancement_score ~FinalEFdata$gender, data= FinalEFdata)
result

#RUN A 2-SAMPLE T-TEST (TO DETERMINE WHETHER OVERALL SUPPRESSION DIFFERS BETWEEN GENDERS)
result <- t.test(FinalEFdata$OverallSuppression_score ~FinalEFdata$gender, data= FinalEFdata)
result

#RUN A 1-WAY ANOVA (TO DETERMINE WHETHER EF DIFFERS BETWEEN AGE)
#step 1: create different groups for age ranges
FinalEFdata$agegroup = with(FinalEFdata, ifelse(age <= 35,"age<=35", ifelse(age <= 64, "age36-64", "age>65")))

#step 2: check to see if the age groups are correct
paste(FinalEFdata$agegroup, "age=" ,FinalEFdata$age)      

#step 3: set the age groups as a factor (factor= independent variable)
FinalEFdata$agegroup= factor(FinalEFdata$agegroup)

#step 4: check to see if the age groups function as a factor
is.factor(FinalEFdata$agegroup)

#step 5: run a 1-way anova between EF & Age
one.way <-aov(FinalEFdata$OverallFlexibility_score ~ agegroup, data = FinalEFdata)
summary(one.way)

#RUN A POST-HOC TEST BETWEEN EF & AGE [TO DETERMINE WHICH VARIABLES ARE SIGNIFICANT]
TukeyHSD(one.way)

#RUN A 2-WAY ANOVA BETWEEN OVERALL FLEXIBILITY, GENDER & AGE 
#step 1: set male & female as categories within gender
gendercat <- factor(FinalEFdata$gender, levels = c(0,1), labels = c("male","female"))

two.way <-aov(OverallFlexibility_score ~ gendercat + agegroup, data = FinalEFdata)
summary(two.way)

#RUN AN INTERACTION EFFECT BETWEEN GENDER & AGE
interaction <- aov(OverallFlexibility_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(OverallEnhancement_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(PositiveEnhancement_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(NegativeEnhancement_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(OverallSuppression_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(PositiveSuppression_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)
interaction <- aov(NegativeSuppression_score ~ gendercat*agegroup, data = FinalEFdata)
summary (interaction)

#CALCULATE THE EXPRESSIVE SUPPRESSION SUBSCALE SCORE [FROM THE ERQ SCALE]
AvgofColumn <-c('erq2', 'erq4', 'erq6', 'erq9')
ColumnNameofAvg <- c('PreferenceforExpressiveSuppression_score')
FinalEFdata[,ColumnNameofAvg] <-rowMeans(FinalEFdata[,AvgofColumn],na.rm = TRUE)

#check to see if the averages have been calculated correctly
(FinalEFdata$PreferenceforExpressiveSuppression_score)

#CALCULATE THE SATISFACTION WITH LIFE SCALE [FROM THE SWLS SCALE]
#step 1: calculate the average for the SWLS scale
AvgofColumn = c('swls1','swls2','swls3','swls4','swls5')
ColumnNameofAvg = c('AverageofSWLS_score')
FinalEFdata[,ColumnNameofAvg] = rowMeans(FinalEFdata[,AvgofColumn],na.rm = TRUE)

#step 2: multiply the average by the number of columns
FinalEFdata[,ColumnNameofAvg]*5
ColumnNameofSum = c('LifeSatisfaction_score')
FinalEFdata[,ColumnNameofSum] = FinalEFdata[,ColumnNameofAvg]*5
LifeSatisfaction_score = c('LifeSatisfaction_score')

#CALCULATE THE SOCIAL SUPPORT SCALE [FROM THE SOCSUP SCALE]
#step 1: calculate the average for the SOCSUP scale
AvgofColumn = c('socsup1','socsup2','socsup3')
ColumnNameofAvg = c('AverageofReceivingEmotionalSupport_score')
FinalEFdata[,ColumnNameofAvg] = rowMeans(FinalEFdata[,AvgofColumn],na.rm = TRUE)
AverageofReceivingEmotionalSupport_score = c('AverageofReceivingEmotionalSupport_score')

#step 2: multiply the average by the number of columns
ColumnNameofSum = c('ReceivingEmotionalSupport_score')
FinalEFdata[,ColumnNameofSum] = FinalEFdata[,ColumnNameofAvg]*3

#RUN A MODERATION ANALYSIS (TO DETERMINE WHETHER ERQ IS A MODERATION VARIBLE BETWEEN SOCSUP & SWLS)
#step 1: run a simple regression between SOCSUP & SWLS
lm_regression = lm(formula = LifeSatisfaction_score ~ AverageofReceivingEmotionalSupport_score, data = FinalEFdata)
summary(lm_regression)

#step 2: compute the interaction effect between ERQ & SWLS
lm_moderation= lm(formula = LifeSatisfaction_score ~ ReceivingEmotionalSupport_score + PreferenceforExpressiveSuppression_score + ReceivingEmotionalSupport_score*PreferenceforExpressiveSuppression_score, data = FinalEFdata)
summary(lm_moderation)

#CREATE A LINEAR REGRESSION PLOT FOR ERQ, SOCSUP & SWLS
install.packages("rockchalk")
library(rockchalk)
ps = plotSlopes(lm_moderation, plotx = "ReceivingEmotionalSupport_score", modx = "PreferenceforExpressiveSuppression_score", xlab= "ReceivingEmotionalSupport", ylab= "LifeSatisfaction_score", modxVals = "std.dev")

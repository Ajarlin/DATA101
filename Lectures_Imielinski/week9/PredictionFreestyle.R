train<-read.csv("M2017_train.csv")
test<-read.csv("M2017_test_students.csv")
submission <-read.csv("M2017_sample_submission.csv")

decision <- rep('F',nrow(train))
decision[train$SCORE>30] <- 'D'
decision[train$SCORE>60 & train$PARTICIPATION>0.5] <- 'C'
decision[train$SCORE>70] <- 'B'
decision[train$SCORE>80] <- 'A'
myprediction<-train
myprediction$GRADE <-decision
error <- mean(train$GRADE!= myprediction$GRADE)
error
#This is just testing on training data! Not predictive!
#Thus we need to crossvalidate - leave out part of train data for testing


library("caTools")
spl <- sample.split(train$GRADE, SplitRatio = 0.7)
#Randomly splits into two subsets 0.7 and 0.3 of the original file
#into training and testing
#We split training data into trainCV and testCV
trainCV <- subset(train, spl == TRUE)
testingCV <- subset(train, spl ==FALSE)
dim(trainCV)
dim(testingCV)
decision <- rep('F',nrow(trainCV))
decision[trainCV$SCORE>30] <- 'D'
decision[trainCV$SCORE>60 & trainCV$PARTICIPATION>0.5] <- 'C'
decision[trainCV$SCORE>70] <- 'B'
decision[trainCV$SCORE>80] <- 'A'
myprediction<-trainCV
myprediction$GRADE <-decision
error <- mean(trainCV$GRADE!= myprediction$GRADE)
error
#this was the error on training subset of training data in crossvalidation. Not so important
#next one is more important - we truly testing on data we did not "train on"
decision <- rep('F',nrow(testingCV))
decision[testingCV$SCORE>30] <- 'D'
decision[testingCV$SCORE>60 & testingCV$PARTICIPATION>0.5] <- 'C'
decision[testingCV$SCORE>70] <- 'B'
decision[testingCV$SCORE>80] <- 'A'
myprediction<-testingCV
myprediction$GRADE <-decision
error <- mean(testingCV$GRADE!= myprediction$GRADE)
error
#This error is far more meaningful. If you not satisfied
#MODIFY YOUR MODEL!


#But assume you are happy with your model and you want to 
#submit your prediction to Kaggle. You need to produce .csv
#file of your predictions
#Remember test is the final testing file now
decision <- rep('F',nrow(test))
decision[test$SCORE>30] <- 'D'
decision[test$SCORE>60 & test$PARTICIPATION>0.5] <- 'C'
decision[test$SCORE>70] <- 'B'
decision[test$SCORE>80] <- 'A'
myprediction<-test
myprediction$GRADE <-decision
outcome<-data.frame(ID=submission$STUDENTID,GRADE=myprediction$GRADE)
#here you just produced two-column data frame with your
#final predictions
write.csv(outcome,"March28_submission.csv",row.names = F)


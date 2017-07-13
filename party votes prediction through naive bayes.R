library(mlbench)
data("HouseVotes84")
library(caret)

#barplots for specific issue
plot(as.factor(HouseVotes84[,2]))
title(main="Votes cast for issue", xlab="vote", ylab="# reps")

#by party
plot(as.factor(HouseVotes84[HouseVotes84$Class=='republican',2]))
title(main="Republican votes cast for issue 1???, xlab="vote", ylab="# reps")
plot(as.factor(HouseVotes84[HouseVotes84$Class=='democrat',2]))
title(main="Democrat votes cast for issue 1???, xlab="vote", ylab="# reps")
      
#function to return number of NAs by vote and class (democrat or republican)
na_by_col_class <- function (col,cls){return(sum(is.na(HouseVotes84[,col]) & HouseVotes84$Class==cls))}

#function to compute the conditional probability that a member of a party will cast a 'yes' vote
p_y_col_class <- function(col,cls){
              sum_y<-sum(HouseVotes84[,col]=='y' & HouseVotes84$Class==cls,na.rm = TRUE)
              sum_n<-sum(HouseVotes84[,col]=='n' & HouseVotes84$Class==cls,na.rm = TRUE)
              return(sum_y/(sum_y+sum_n))}

#impute missing values.
for (i in 2:ncol(HouseVotes84)) {
        if(sum(is.na(HouseVotes84[,i])>0)) {
                c1 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=='democrat',arr.ind = TRUE)
                c2 <- which(is.na(HouseVotes84[,i])& HouseVotes84$Class=='republican',arr.ind = TRUE)
                HouseVotes84[c1,i] <-
                        ifelse(runif(na_by_col_class(i,'democrat'))<p_y_col_class(i,'democrat'),'y','n')
                HouseVotes84[c2,i] <-
                        ifelse(runif(na_by_col_class(i,'republican'))<p_y_col_class(i,'republican'),'y','n')}
}

#create new col "train" and assign 1 or 0 in 80 and 20 proportion
HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<0.80,1,0)

#get col number of train 
trainColNum <- grep("train",names(HouseVotes84))

#separate training and test sets and remove training column
trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]

#load e1071 library and invoke naiveBayes method
library(e1071)
nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)

nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])
#confusion matrix
table(pred=nb_test_predict,true=testHouseVotes84$Class)

#fraction of correct predictions
mean(nb_test_predict==testHouseVotes84$Class
     

#function to create, run and record model results
nb_multiple_runs <- function(train_fraction,n){
             fraction_correct <- rep(NA,n)
             for (i in 1:n){
                     HouseVotes84[,"train"] <- ifelse(runif(nrow(HouseVotes84))<train_fraction,1,0)
                     trainColNum <- grep("train",names(HouseVotes84))
                     trainHouseVotes84 <- HouseVotes84[HouseVotes84$train==1,-trainColNum]
                     testHouseVotes84 <- HouseVotes84[HouseVotes84$train==0,-trainColNum]
                     nb_model <- naiveBayes(Class~.,data = trainHouseVotes84)
                     nb_test_predict <- predict(nb_model,testHouseVotes84[,-1])
                     fraction_correct[i] <- mean(nb_test_predict==testHouseVotes84$Class)
             }
             return(fraction_correct)
}



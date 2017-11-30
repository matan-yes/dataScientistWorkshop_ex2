# Assignment 2 Titanic data set
## Titanic: Machine Learning from Disaster
---
Title: "Assignment2_Titanic"
Author: "Matan Yeshurun & Alon Galperin"
Lecturer: "Sigal Shaked"
date: "November 20, 2017"
output: html_document
---
###The Data

We will use the titanic data, based on kaggle's [Machine Learning from Disaster](https://www.kaggle.com/c/titanic) competition. 
The titanic sank after colliding with an iceberg, killing 1502 out of 2224 passengers and crew. 
Although there was some element of luck involved in surviving the sinking, some groups of people were more likely to get on a lifeboat than others, such as women, children and upper-class.
In this task, you will apply the tools of machine learning to predict which passengers survived the tragedy.

###Variables Description:


|Feature |  Description|
|-------------|:------------------------------------------------|
|survival |  Survival (0 = No; 1 = Yes)|
|pclass |  Passenger Class (1 = 1st; 2 = 2nd; 3 = 3rd)|
|name  |  Name|
|sex |   Sex|
|age |  Age|
|sibsp  |   Number of Siblings/Spouses Aboard|
|parch  |   Number of Parents/Children Aboard|
|ticket |   Ticket Number|
|fare  |   Passenger Fare|
|cabin  |   Cabin|
|embarked |   Port of Embarkation (C = Cherbourg; Q = Queenstown; S = Southampton)|







In this exercise we will work with the Titanic dataset.

1. Set working directory to be the path: c:\titanic
```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE)
setwd('C://titanic')
```

2. Read the train.csv file into a dataframe.
   We will set all the empty cells in our dataset to be na
   we also read our test set, add a Survived column with empty values for making some data preperation, before using in classification models
   
   at the end of this part we will have dataframe named "dataframe" that will contain the records from both train and test sets
```{r}
dataframe <- read.csv('train.csv',na.strings = "")
train_rows <- nrow(dataframe)
dataframe_test <- read.csv('test.csv',na.strings = "") 
dataframe_test$Survived<-NA # adding survived empty column. reason: to have equal number of columns in 2 train and test sets for cbind
dataframe <- rbind(dataframe,dataframe_test)
dataframe_test <- NA # delete the test set for now, we will create it later
```
 
3. We will print the structure of the dataframe and learn which kind of there are attributes in the dataset. We also print the 5 first records.
Check the datatypes of the attributes using the *str* method.
```{r}
str(dataframe)
head(dataframe, 5)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/1-str.JPG)
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/2-head.JPG)


4. We can see that the survived attribute is int. It is preffered for most algorithms that the target attibute will be of type factor.
### We will change Survived attribute from int to factor, and also change Pclass from int to factor.
```{r}

dataframe$Survived<- as.factor(dataframe$Survived)
dataframe$Pclass<- as.factor(dataframe$Pclass)

```

In the dataset we have attributes that are noisy to the decision algorithms.
This noisy attribtes are: passangerID, ticketID and embarked, we will remove those attributes.

## Data Exploration

6. tidy the data
we will tidy the data using the library tidyr.
It is easier to explore factors and numeric features separately. Here we divide the features' names to numerics and factors:
- first we devide the features of the dataset to numerics and factors
- second we  apply the gther function on the factors
- third we apply apply the gther function on the numeric

```{r}
library(tidyr)
dataframe_explore <- head(dataframe[-1], train_rows)
cols<- 1:dim(dataframe_explore)[2]
factors <- cols[sapply(dataframe_explore,is.factor)]
numerics <- cols[!sapply(dataframe_explore,is.factor)]
df_tidy_factors<-gather(dataframe_explore[,factors],"feature","value",-1)
df_tidy_numerics<-gather(cbind(Survived=dataframe_explore[,1],dataframe_explore[,numerics]),"feature","value",-1)

```
7. 
Finally, we can plot
The first plot describes only categorical features (factors)

```{r}
#install.packages("ggplot2")
library(ggplot2)
qplot(x=value,data=df_tidy_factors,fill=Survived) + facet_grid(~feature,scales="free")
```
![Data Exploration Factor Image](https://github.com/matan-yes/ex2/blob/master/images/3-qplot_factors.JPG)

One more plot for numeric features:
```{r}
qplot(x=value,data=df_tidy_numerics,fill=Survived) + facet_grid(~feature,scales="free")
```
![Data Exploration Numeric Image](https://github.com/matan-yes/ex2/blob/master/images/4-qplot_numeric.JPG)

## Feature engineering

1. Seperate the Personal Titles from the passenger names at the next steps we will remove the names and remain with the Personal Titles only as followes.
Sir - gather all man respectful titles
Lady - gather all women respectful titles
Mlle - In english equals to Miss
Mme - In english equals to Ms
```{r}
dataframe$PersonalTitles <- mapply(function(x) {strsplit(x, '[,.]')[[1]][2]}, as.character(dataframe$Name))
dataframe$PersonalTitles[dataframe$PersonalTitles %in% c(' Capt',' Col', ' Don', ' Major', ' Sir')] <- ' Sir'
dataframe$PersonalTitles[dataframe$PersonalTitles %in% c(' Jonkheer',' Dona', ' Lady', ' the Countess')] <- ' Lady'
dataframe$PersonalTitles[dataframe$PersonalTitles %in% c(' Mlle')] <- ' Miss'
dataframe$PersonalTitles[dataframe$PersonalTitles %in% c(' Mme')] <- ' Ms'
dataframe$PersonalTitles<-as.factor(dataframe$PersonalTitles)
table(dataframe$PersonalTitles)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/5-feature_titles.JPG)
2.Seperate the Tickets Prefix from the passenger Tickets at the next steps we will remove the Tickets and remain with the prefix Titles only as followes.
```{r}
dataframe$TicketPrefix <- mapply(function(x) {strsplit(x, '\\s+')[[1]]}, as.character(dataframe$Ticket))
dataframe$TicketPrefix <- mapply(function(x) {ifelse(length(x)>1, x[1], NA)}, dataframe$TicketPrefix)
dataframe$TicketPrefix <- mapply(function(x) {gsub('[./,]','', x)}, dataframe$TicketPrefix)
dataframe$TicketPrefix <- mapply(function(x) {toupper(x)}, dataframe$TicketPrefix)
dataframe$TicketPrefix <- as.factor(dataframe$TicketPrefix)
table(dataframe$TicketPrefix)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/6-feature_ticket_prefix.JPG)
3.Seperate the family name from the passenger names
```{r FamilyName}
FamilyName <- mapply(function(x) {strsplit(x, '[,.]')[[1]][1]}, as.character(dataframe$Name))
```

4.sibSp - number of siblings / spouses aboard the Titanic
parch - number of parents / children aboard the Titanic
FamilySize = sibSp + parch + 1
```{r FamilySize}
dataframe$FamilySize <- mapply(function(sibSp, parch) { sibSp + parch + 1}, dataframe$SibSp, dataframe$Parch)
table(dataframe$FamilySize)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/7-feature_family.JPG)

5.Combine Family Size and Family Name to new feature
```{r}
dataframe$FamilySizeSurName <- mapply(function(familyS, surN) { paste(as.character(familyS), as.character(surN), sep='')}, dataframe$FamilySize, FamilyName)
FamilySizeSurNameCount<-as.data.frame(table(dataframe$FamilySizeSurName))

dataframe$FamilySizeSurName <- mapply(function(familySS) { 
  (FamilySizeSurNameCount[which(FamilySizeSurNameCount$Var1 == familySS),]$Freq -mean(FamilySizeSurNameCount$Freq)) /sqrt(var(FamilySizeSurNameCount$Freq))
  }, dataframe$FamilySizeSurName)

```

6.The cabin room number can be meaningful so we created new feature with the passenger room number.
```{r CabinMinRoomNumber}
dataframe$CabinMinRoomNumber<-mapply(function(x)(gsub('[a-zA-Z]', '', x)), dataframe$Cabin)
dataframe$CabinMinRoomNumber <- mapply(function(x) {strsplit(x, '\\s+' )[[1]][1]}, as.character(dataframe$CabinMinRoomNumber))
dataframe$CabinMinRoomNumber <- as.integer(dataframe$CabinMinRoomNumber)
table(dataframe$CabinMinRoomNumber)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/8-feature_room_number.JPG)

7.The cabin level can be meaningful so we created new feature with the cabin passenger level .
```{r CabinLevel}
dataframe$CabinLevel<-mapply(function(x)(gsub('[^a-zA-Z]', '', x)), dataframe$Cabin)
dataframe$CabinLevel <- mapply(function(x) {substr(x,1,1)}, as.character(dataframe$CabinLevel))
dataframe$CabinLevel <- as.factor(dataframe$CabinLevel)
table(dataframe$CabinLevel)
```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/9-feature_cabin_level.JPG)
### Check NA's values
Lets run summary to check how much NA's we have
```{r}

apply(is.na(dataframe),2,sum)


```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/10-feature_summery.JPG)
we can see that 'Age' have 263 missing values cabin has 1014 missing values and PersonalTitles has 15 Fare has 1 and more...

### Compleate missing values
Lets complete these values using rpart algorithm:

1.Predict missing values for Age Column using 'anova' method because it contain continues value
```{r}
library('rpart')
Agefit <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + PersonalTitles + FamilySize,
                  data=dataframe[!is.na(dataframe$Age),], 
                  method="anova")
dataframe$Age[is.na(dataframe$Age)] <- predict(Agefit, dataframe[is.na(dataframe$Age),])
```


2.Predict missing values for Ticket prefix coulumn using 'class' method because it contain nominal value
```{r}
TicketPrefixfit <- rpart(TicketPrefix ~ Pclass + Age + Sex + SibSp + Parch + Fare + Embarked + PersonalTitles + FamilySize,
                  data=dataframe[!is.na(dataframe$TicketPrefix),], 
                  method="class")
dataframe$TicketPrefix[is.na(dataframe$TicketPrefix)] <- predict(TicketPrefixfit, dataframe[is.na(dataframe$TicketPrefix),],type = "class")
```


3.Predict missing values for Fare Column using 'anova' method because it contain continues value
```{r}
Farefit <- rpart(Fare ~ Pclass + Sex + SibSp + Parch + Age + Embarked + PersonalTitles + FamilySize,
                  data=dataframe[!is.na(dataframe$Fare),], 
                  method="anova")
dataframe$Fare[is.na(dataframe$Fare)] <- predict(Farefit, dataframe[is.na(dataframe$Fare),])
```

4.Predict missing values for Embarked coulumn using 'class' method because it contain nominal value
```{r}
Embarkedfit <- rpart(Embarked ~ Pclass + Age + Sex + SibSp + Parch + Fare + PersonalTitles + FamilySize,
                  data=dataframe[!is.na(dataframe$Embarked),], 
                  method="class")
dataframe$Embarked[is.na(dataframe$Embarked)] <- predict(Embarkedfit, dataframe[is.na(dataframe$Embarked),],type = "class")
```

5.Predict missing values for Cabin Min Room Number Column using 'anova' method because it contain continues value
```{r}
CabinMinRoomNumberfit <- rpart(CabinMinRoomNumber ~ Pclass + Sex + SibSp + Parch + + Fare+ Age + Embarked + PersonalTitles + FamilySize,
                  data=dataframe[!is.na(dataframe$CabinMinRoomNumber),], 
                  method="anova")
dataframe$CabinMinRoomNumber[is.na(dataframe$CabinMinRoomNumber)] <- predict(CabinMinRoomNumberfit, dataframe[is.na(dataframe$CabinMinRoomNumber),])
```


6.Predict missing values for Cabin Level  coulumn using 'class' method because it contain nominal value
```{r}
CabinLevelfit <- rpart(CabinLevel ~ Pclass + Sex + Fare + Age + PersonalTitles,
                  data=dataframe[!is.na(dataframe$CabinLevel),], 
                  method="class")
dataframe$CabinLevel[is.na(dataframe$CabinLevel)] <- predict(CabinLevelfit, dataframe[is.na(dataframe$CabinLevel),],type = "class")
```


7.Normalize the data in  fare feature (z -score normalization)
```{r}
dataframe$Fare<-mapply(function(fare){(fare - mean(dataframe$Fare))/sqrt(var(dataframe$Fare))}, dataframe$Fare)
```

##Now just before we will go on to the next step and begin to train model we will get rid from some unnecessary features
PassengerId,Name,Ticket,Cabin and split our data back to train and test 
```{r}

dataframe_backup <- dataframe # backup the 

# create dataframe that will backup the passengerIds before we delete them from the test set
#passengersIds <- tail(dataframe$PassengerId, -train_rows)
#passengersIds <- data.frame(passengersIds)
passengersIds <- tail(dataframe_backup$PassengerId, -train_rows)

# remove noisy
dataframe <- dataframe[,-c(1,4, 9, 11,14)]
dataframe_train<-head(dataframe, train_rows)
dataframe_test<-tail(dataframe, -train_rows)[-1] # [-1] that means that we delete the passengerID attribute

```
##This is our well preperd data set for trainig our model. We can see that the noisy attributes got removed
```{r}

str(dataframe_train)
head(dataframe_train, 5)

```
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/11-str_df_train.JPG)
![str result Image](https://github.com/matan-yes/ex2/blob/master/images/12-head_df_train.JPG)


### Training models
##r-part
# 1. Let's train an rpart model based on the trainset:

##DATASET 
We used our dataset after some changes we have made as listed at the section above 

#Algorithm rpart decision tree (Learned at class)
```{r}
library(rpart)
library(caret)
library(party)

set.seed(123)
control <- trainControl(method="cv", number=10, repeats=3)

rpartfit <- train(
        Survived~., data= dataframe_train, method="rpart", trControl=control, 
          tuneGrid = expand.grid(
            cp = c(0.01,0.005,0.0005)
          ))

plot(rpartfit)
```
![r-part plot](https://github.com/matan-yes/ex2/blob/master/images/13-rpart_plot.JPG)
it seems like 0.010 give the best acuuracy.

#### Feature Importance
```{r}
varImp(rpartfit)
```
![r-part feature importance](https://github.com/matan-yes/ex2/blob/master/images/14-rpart_var_importance.JPG)

#### predection
```{r}
rpart_prediction <- predict(rpartfit, dataframe_test)
```

write to file
```{r}
result<-as.numeric(rpart_prediction)
result[result==1]<-0
result[result==2]<-1
result_to_file <- cbind(PassengerId=passengersIds ,Survived=result)
write.csv(result_to_file,file="rpart_statistics_result.csv",row.names = F)
```
### Result -  Submitted file

[link to submitted file!](https://github.com/matan-yes/ex2/blob/master/results/rpart_statistics_result.csv)

### Screenshot of Kaggle r-part submission

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_rpart_submission.JPG)

### Screenshot of Kaggle r-part rank

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_rpart_rank.JPG)

##Random Forest
# 1. Let's train an Random Forest model based on the trainset:

##DATASET 
We used our dataset after some changes we have made as listed at the section above 


# Caret using Random Forest algorithm
```{r}

control <- trainControl(method="cv", number=3)

set.seed(7)
fit.rf <- train(x= dataframe_train[,-1] ,y=dataframe_train[,1], method="rf", ntree=2000, trControl=control)

test <- dataframe_train[1:100,]
pred_random_forest <- predict(fit.rf, newdata = dataframe_test)

# write to disk

passengersIds <- tail(dataframe_backup$PassengerId, -train_rows)

result<-as.numeric(pred_random_forest)
result[result==1]<-0
result[result==2]<-1
result_to_file <- cbind(PassengerId=passengersIds,Survived=result)
write.csv(result_to_file,file="random_forest_result.csv",row.names = F)

```
### Result -  Submitted file

[link to submitted file!](https://github.com/matan-yes/ex2/blob/master/results/random_forest_result.csv)

### Screenshot of Kaggle random forest submission

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_random_forest_submission.JPG)

### Screenshot of Kaggle random forest rank

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_random_forest_rank.JPG)

#Naive Bayes

```{r}

# Naive Bayes

library(e1071)
nb_model <- naiveBayes(Survived~.,data = dataframe_train)
nb_test_predict <- predict(nb_model, dataframe_test)
#table(pred=nb_test_predict,true=dataframe_train$Survived)

#mean(nb_test_predict==dataframe_train$Survived)

passengersIds <- tail(dataframe_backup$PassengerId, -train_rows)

result<-as.numeric(nb_test_predict)
result[result==1]<-0
result[result==2]<-1
result_to_file <- cbind(PassengerId=passengersIds, Survived=result)
write.csv(result_to_file,file="naive_bayes.csv",row.names = F)

```
![prediction](https://github.com/matan-yes/ex2/blob/master/images/nb_prediction.JPG)
### Result -  Submitted file

[link to submitted file!](https://github.com/matan-yes/ex2/blob/master/results/naive_bayes.csv)

### Screenshot of Kaggle random forest submission

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_naive_bayes_submission.JPG)

### Screenshot of Kaggle random forest rank

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_naive_bayes_rank.JPG)


##gbm Statistics
# 1. Let's train an gbm model based on the trainset:

##DATASET 
We used our dataset after some changes we have made as listed at the section above 

#### gbm Statistics
```{r}
library(stats)
set.seed(123)

trcontrol <- trainControl(method="cv", number=10, repeats=3)

gbmfit <- train(
        Survived~., data=dataframe_train, method="gbm", trControl=trcontrol, 
          tuneGrid = expand.grid(
            n.trees = c(1000,2000), 
            interaction.depth = c(5,10,20),
            shrinkage = c(0.001, 0.01, 0.1),
            n.minobsinnode = c(10, 20, 30)
          ),
        verbose = FALSE)


plot(gbmfit)
```
![Plot gbmfit Image](https://github.com/matan-yes/ex2/blob/master/images/gbm_plot.jpg)

We can see that the best tuning is shrinkage = 0.01 and interaction.depth is 10. n.minobsinnode = 30

lets test more ntree options:

```{r}
library(stats)
set.seed(123)

trcontrol <- trainControl(method="cv", number=10, repeats=3)

gbmfit <- train(
        Survived~Sex + Age + Embarked + FamilySize + PersonalTitles + FamilySizeSurName + CabinMinRoomNumber, data=dataframe_train, method="gbm", trControl=trcontrol, 
          tuneGrid = expand.grid(
            n.trees = c(300, 500, 1000, 1500,2000), 
            interaction.depth = 10,
            shrinkage = 0.01,
            n.minobsinnode = 30
          ),
        verbose = FALSE)


plot(gbmfit)
```
![Plot gbmfit Image](https://github.com/matan-yes/ex2/blob/master/images/gbm_fit_plot.jpg)

great ntree = 300 is the best score.


### Predict
```{r}
gmb_pred<- predict(gbmfit,newdata = dataframe_test)
gmb_pred<-sapply(gmb_pred, function(x){ as.character(x) == "1" })
res <- cbind(PassengerId=passengersIds,Survived=gmb_pred)
write.csv(res,file="gbm_results.csv",row.names = F)
```

### Result -  Submitted file

[link to submitted file!](https://github.com/matan-yes/ex2/blob/master/results/gbm_results.csv)

### Screenshot of Kaggle gbm submission

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_gbm_results_submission.JPG)

### Screenshot of Kaggle gbm rank

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_gbm_results_rank.JPG)



# Ensemble (cforest,gbm,rpart)


### Algorithm Description
We will use in ensemble of the 3 models ():
1. cforest - An implementation of the random forest and bagging ensemble algorithms utilizing conditional inference trees as base learners.
2. rpart - Decision trees.
3. gbm - Stochastic Gradient Boosting.

The stacking algorithm will be RandomForest.

### Algorithm Calibration
We already calibrated the parameters of each algorithm separately.

Let's look that there is no high colleration between them:

```{r}
library(caret)
#install.packages("caretEnsemble")
library(caretEnsemble)
#install.packages("party")
library(party)

set.seed(123)
control <- trainControl(method="cv", number=10, savePredictions='final', classProbs=TRUE)

models <- caretList(
  make.names(Survived)~., 
  data=dataframe_train,
  metric='Accuracy', 
  trControl=control,
  tuneList = list(
    cforest = caretModelSpec(
      method = "cforest",
      controls = cforest_unbiased(ntree=2000, mtry=3)  
    ),
    gbm = caretModelSpec(
      method = "gbm",
      tuneGrid = expand.grid(
        n.trees = 2000, 
        interaction.depth = 10,
        shrinkage = 0.01,
        n.minobsinnode = 30
        ),
      verbose = FALSE
    ),
    rpart = caretModelSpec(
      method = "rpart",
      tuneGrid = expand.grid(
          cp = 0.005
        )
    )
  )
  )
results <- resamples(models)
summary(results)
modelCor(results)
```
![Summery](https://github.com/matan-yes/ex2/blob/master/images/ensamble_summery.JPG)
![Model corelation](https://github.com/matan-yes/ex2/blob/master/images/ensamble_model_corelation.JPG)


We can see that there is no high correlation (<75) between them and we can continue to build one model from all the models:

```{r}
set.seed(123)
stackControl <- trainControl(method="cv", number=10, savePredictions='final', classProbs=TRUE)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
#, tuneGrid =expand.grid(mtry = c(2, 3, 6, 9)))
print(stack.rf)
```
![str result Image]((https://github.com/matan-yes/ex2/blob/master/images/ensamble_mtray.JPG)

### Predict Test File and Write results

```{r}
stack_prediction<- predict(stack.rf,newdata = dataframe_test)

stack_prediction<-sapply(stack_prediction, function(x){ as.character(x) == "X1" })
```

Write the *PassengerId* and *Survived* attributes to a csv file and submit this file to kaggle's competition 

```{r}
res <- cbind(PassengerId=passengersIds,Survived=stack_prediction)
write.csv(res,file="ensemble_result.csv",row.names = F)
```

### Result -  Submitted file

[link to submitted file!](https://github.com/matan-yes/ex2/blob/master/results/ensemble_result.csv)

### Screenshot of Kaggle ensemble submission

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_ensembles_submission.JPG)

### Screenshot of Kaggle ensemble rank

![Leaderboard Image](https://github.com/matan-yes/ex2/blob/master/images/Kaggel_ensemble_rank.JPG)




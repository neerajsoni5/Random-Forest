#https://www.r-bloggers.com/how-to-implement-random-forests-in-r/

library(data.table)
# Load the dataset and explore
data1 <- fread("file:///C:/Users/SoniNe02/Desktop/Others/Final/RandomForest/d.txt")
names(data1)<-c("BuyingPrice",
"Maintenance",
"NumDoors",
"NumPersons",
"BootSpace",
"Safety",
"Condition" )

str(data1)
upd.cols = sapply(data1, is.character)
#data1<-sapply(data1,is.factor)

data1[, names(data1)[upd.cols] := lapply(.SD, as.factor), .SDcols = upd.cols]

summary(data1)
# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

#Now, we will create a Random Forest model with default parameters and then we will fine tune the model by changing 'mtry'. 
#We can tune the random forest model by changing the number of trees (ntree) and the number of variables randomly sampled 
#at each stage (mtry). According to Random Forest package description:

#Ntree: Number of trees to grow. This should not be set to too small a number,
#to ensure that every input row gets predicted at least a few times.

#Mtry: Number of variables randomly sampled as candidates at each split. 
#Note that the default values are different for classification (sqrt(p) where p is number of variables in x) 
#and regression (p/3) 

library(randomForest)
# Create a Random Forest model with default parameters
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1



# Fine tuning parameters of Random Forest model
model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)
model2

# Predicting on train set
predTrain <- predict(model2, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Condition)  


# Predicting on Validation set
predValid <- predict(model2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$Condition)                    
table(predValid,ValidSet$Condition)

#In case of prediction on train dataset, there is zero misclassification; however
#the case of validation dataset, 6 data points are misclassified and accuracy is 98.84%
#We can also use function to check important variables. The below functions show the drop in mean accuracy for each of the variables.

# To check important variables
importance(model2)        
varImpPlot(model2)   

#Now, we will use 'for' loop and check for different values of mtry.
# Using For loop to identify the right mtry for model
a=c()
i=5
for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i-2] = mean(predValid == ValidSet$Condition)
}

a

plot(3:8,a)

#From the above graph, we can see that the accuracy decreased when mtry was increased from 4 to 5 and then increased when mtry was changed to 6 from 5. 


###Let's compare this model with decision tree and see how decision trees fare in comparison to random forest.

library(rpart)
library(caret)
library(e1071)
# We will compare model 1 of Random Forest with Decision Tree model

model_dt = train(Condition ~ ., data = TrainSet, method = "rpart")
model_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)

mean(model_dt_1 == TrainSet$Condition)


# Running on Validation Set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Condition)

mean(model_dt_vs == ValidSet$Condition)
#The accuracy on validation dataset has decreased further to 77.6%.



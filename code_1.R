rm(list = ls(all = T))

setwd("C:\\Users\\yogesh\\Desktop\\Insofe\\Hackathon\\LivSpace")   #setting working directory
Data = read.table("car.data.txt", sep = ",")   #Importing data
str(Data)    #getting the data structure
colnames(Data) = c('buying',
                   'maint',
                   'doors',
                   'persons',
                   'lug_boot',
                   'safety',
                   'Class')   # setting column names



#Data$code = NULL   #removing id column
#Data$Class = as.factor(Data$Class)

#exploring the data
head(Data)
table(Data$Class)
summary(Data)

# checking for missing values
perc_missing = colSums(is.na(Data))/nrow(Data) 
perc_missing
sum(is.na(Data))




#Sampling the data into test and train

require(caTools)
set.seed(123)
sample_rows = sample.split(Data$Class, SplitRatio = 0.8)

train = subset(Data, sample_rows == TRUE)


test = subset(Data, sample_rows == FALSE)



nrow(train)

table(train$Class)





#Running the RandomForest classifier model
library(randomForest)
set.seed(123)
rf = randomForest(Class ~ ., data = train, ntree = 600)
summary(rf)
rf
pred = predict(rf, test)   # predicting the model on test data
tb = table(pred,test$Class) #confusion matrix

accur = sum(diag(tb))/nrow(test)   # classification accuracy of the model
accur



#class      N          N[%]
-----------------------------
#  unacc     1210     (70.023 %) 
# acc        384     (22.222 %) 
# good        69     ( 3.993 %) 
# v-good      65     ( 3.762 %) 

  
  

  
  




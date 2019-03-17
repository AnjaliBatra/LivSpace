rm(list = ls(all = T))

setwd("C:\\Users\\yogesh\\Desktop\\Insofe\\Hackathon\\LivSpace")   #setting working directory
Data = read.table("breast-cancer-wisconsin.data.txt", sep = ",")   #Importing data
str(Data)    #getting the data structure
colnames(Data) = c("code",'Clump_Thickness',
                   'Uniformity_of_Cell_Size',
                   'Uniformity_of_Cell_Shape',
                   'Marginal_Adhesion',
                   'Single_Epithelial_Cell_Size',
                   'Bare_Nuclei',
                   'Bland_Chromatin',
                   'Normal_Nucleoli',
                   'Mitoses',
                   'Class')   # setting column names

#Setting the data types of columns
Data$Clump_Thickness=as.factor(Data$Clump_Thickness)
Data$Uniformity_of_Cell_Size=as.factor(Data$Uniformity_of_Cell_Size)
Data$Uniformity_of_Cell_Shape=as.factor(Data$Uniformity_of_Cell_Shape)
Data$Marginal_Adhesion=as.factor(Data$Marginal_Adhesion)
Data$Single_Epithelial_Cell_Size=as.factor(Data$Single_Epithelial_Cell_Size)
Data$Bare_Nuclei=as.factor(Data$Bare_Nuclei)
Data$Bland_Chromatin=as.factor(Data$Bland_Chromatin)
Data$Normal_Nucleoli=as.factor(Data$Normal_Nucleoli)
Data$Mitoses=as.factor(Data$Mitoses)
Data$Class=as.factor(Data$Class)


Data$code = NULL   #removing id column
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
table(pred,test$Class) #confusion matrix

accur = (89+46)/nrow(test)   # classification accuracy of the model
accur

# 2 - Benign 
# 4 - Malignant







library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(flexclust)


#cluster Analysis for Prediction

train$Clump_Thickness = as.numeric(train$Clump_Thickness)
train$Uniformity_of_Cell_Size = as.numeric(train$Uniformity_of_Cell_Size)
train$Uniformity_of_Cell_Shape = as.numeric(train$Uniformity_of_Cell_Shape)
train$Marginal_Adhesion = as.numeric(train$Marginal_Adhesion)
train$Single_Epithelial_Cell_Size = as.numeric(train$Single_Epithelial_Cell_Size)
train$Bare_Nuclei = as.numeric(train$Bare_Nuclei)
train$Bland_Chromatin = as.numeric(train$Bland_Chromatin)
train$Normal_Nucleoli = as.numeric(train$Normal_Nucleoli)
train$Mitoses = as.numeric(train$Mitoses)


test$Clump_Thickness = as.numeric(test$Clump_Thickness)
test$Uniformity_of_Cell_Size = as.numeric(test$Uniformity_of_Cell_Size)
test$Uniformity_of_Cell_Shape = as.numeric(test$Uniformity_of_Cell_Shape)
test$Marginal_Adhesion = as.numeric(test$Marginal_Adhesion)
test$Single_Epithelial_Cell_Size = as.numeric(test$Single_Epithelial_Cell_Size)
test$Bare_Nuclei = as.numeric(test$Bare_Nuclei)
test$Bland_Chromatin = as.numeric(test$Bland_Chromatin)
test$Normal_Nucleoli = as.numeric(test$Normal_Nucleoli)
test$Mitoses = as.numeric(test$Mitoses)




cl1 = kcca(x = train[-length(train)], k = 2)
cl1
pred_test = predict(cl1, test[-length(test)])

tb = table(pred_test,test$Class)
tb
accur = sum(diag(tb))/nrow(test)
accur


#cluster analysis for visualization
distance <- get_dist(train)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))


k2 <- kmeans(train, centers = 2, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = train)
k2$centers
k2$cluster
k2$size



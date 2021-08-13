#packages party, rpart, rpart.plot
library(party)
library(rpart)
library(rpart.plot)
library(dplyr)
Rpart_data <- read.csv(file.choose(), header = T) #choose "Ctree_test_3.csv"
masterdata <- Rpart_data #833
#filter for college HMCSE
#treedata <- filter(masterdata, masterdata$Stu_College3== "HMCSE") #1041

treedataset_M <- treedataset_M[,-1]

#Fianl data name is ctree_small.csv
#treedata <- Rpart_data
#data partitions
set.seed(1234)
pd <- sample(2, nrow(treedataset_M), replace = TRUE, prob=c(0.8,0.2))
train <- treedataset_M[pd==1,] #659
test <- treedataset_M[pd==2,]#174

#model 01 include Year 4 factors
tree0 <- ctree(Gradu_Code ~ ., data=train, controls = ctree_control(mincriterion = 0.9,minsplit = 10))
tree0
plot(tree0)

#model 02 exclude year 4 factors
treedataset_M <- read.csv(file.choose(), header = T)
treedataset_M <- select(treedataset_M, -1)
glimpse(treedataset_M)
write.csv(treedataset_M,"Rtreedataset_M.csv")


### Ctree analysis Begin
set.seed(1234)
pd <- sample(2, nrow(treedataset_M), replace = TRUE, prob=c(0.8,0.2))
trainDT <- treedataset_M[pd==1,] #659
testDT <- treedataset_M[pd==2,]#174
tree02 <- ctree(Gradu_Code ~ ., data=trainDT, controls = ctree_control(mincriterion = 0.9,minsplit = 10))
tree02
#prediction
train3$p <- as.data.frame(predict(tree02, train3, type="prob"))
head(train3,5)

p1 <- predict(tree02, trainDT, type="prob")
head(cbind(p1, trainDT))
write.csv(train3,"Prediction_DT.csv")
#plot
plot(tree02)
#combine the probability and dataset
library(dplyr)


#prediction
tab_tree02 <- table(predict(tree02), train3$Gradu_Code)
print(tab_tree02)
1-sum(diag(tab_tree02))/sum(tab_tree02)#0.2078

#confusion matrix
library(caret)
confusionMatrix(predict(tree02), train3$Gradu_Code)

#Decision Tree
tree <- ctree(Gradu_Code ~ Stu_ProgramCIPCodeChange+Stu_CollegeChange+Stu_TotalUniversityHoursBegin+Stu_DepartmentChange
              +Stu_TotalUniversityHours3+Stu_TotalInstHours3+Stu_Ethnicity+HoursDWFCount3+Stu_Gender+Deg_Depar.Code
              , data=train, controls = ctree_control(mincriterion = 0.9,minsplit = 10))
tree
plot(tree)

#Train_2

tree2 <- ctree(Gradu_Code ~ , data=train, controls = ctree_control(mincriterion = 0.9,minsplit = 10))
tree2
plot(tree2)

#prediction
 predict(tree, train, type="prob")

 # method is "class"since y is factor

tree1 <- rpart(Gradu_Code ~ Stu_ProgramCIPCodeChange+Stu_CollegeChange+Stu_TotalUniversityHoursBegin+Stu_DepartmentChange
               +Stu_TotalUniversityHours3+Stu_TotalInstHours3+Stu_Ethnicity+HoursDWFCount3+Stu_Gender+Deg_Depar.Code
               , data=train, control = rpart.control(minsplit=20,cp=0.01))
rpart.plot(tree1, extra = 1)
rpart.plot(tree1, extra = 2)
rpart.plot(tree1, extra = 3)
rpart.plot(tree1, extra = 4)
tree1$variable.importance


#text(tree1)



#misclassification train dataset
tab_tree0 <- table(predict(tree0), train$Gradu_Code)
print(tab_tree0)
1-sum(diag(tab_tree0))/sum(tab_tree0)#0.135051

tab_tree <- table(predict(tree), train$Gradu_Code)
print(tab_tree)
1-sum(diag(tab_tree))/sum(tab_tree)

#rpart for train3

tree33 <- rpart(Gradu_Code ~ ., data=train3, control = rpart.control(minsplit=20,cp=0.01))
rpart.plot(tree33, extra = 1)
rpart.plot(tree33, extra = 2)
rpart.plot(tree33, extra = 3)
rpart.plot(tree33, extra = 4)
tree1$variable.importance

#################################################
#### Feature selection using R             ######
#################################################

#based Random forest

# library Boruta mlbench caret randomForest
library("Boruta")
library(mlbench)  
library(randomForest)
library(caret)

#data
CSEdata <- read.csv("~/Data_IR/CSEdata.csv", stringsAsFactors=TRUE); View(CSEdata)
#CSEdata$GPA_ENTERING_SECOND_FALL
str(CSEdata)
apply(CSEdata, 2,p) # find NA



# data set
CSEdataAveGPA <- CSEdata %>% 
  mutate(aveGPA = ifelse(GPA_ENTERING_SECOND_FALL>= 2.77,"above2.77","below2.77")) %>% 
  filter(ENTRY_COLLEGE=="HMCSE") %>% 
  select(#ENTRY_PROGRAM,ENTRY_DEPARTMENT,
    GENDER,ETHNICITY,NATION,aveGPA,APPLICANT_TIER,
    AGE_AT_ENTRY,
    #COUNTY,STATE,
         
         #GPA_HIGHSCHOOL,
    FIRST_FALL_PELL_AMOUNT,FIRST_FALL_BRIGHT_FUTURES_AMOUNT,FIRST_FALL_NEED_BASED_LOANS_AMOUNT,
         FIRST_FALL_NON_NEED_BASED_LOANS_AMOUNT,FIRST_FALL_NEED_BASED_LOANS_AMOUNT,AP_CREDITS,HOURS_BROUGHT_TO_UNIVERSITY) %>% 
    na.omit()

CSEdataAveGPA$aveGPA <- as.factor(CSEdataAveGPA$aveGPA) 
CSEdataAveGPA$APPLICANT_TIER <- as.factor(CSEdataAveGPA$APPLICANT_TIER)
str(CSEdataAveGPA)

apply(CSEdataAveGPA, 2, p)

# check multicollinearity
cordata <- CSEdataAveGPA %>% 
  select(6:12)
glimpse(cordata)
library(PerformanceAnalytics)
chart.Correlation(cordata)

#Feature selection
set.seed(111) #for random saple to generate

#if youhave NAs use rfImpute
data.imputed <- rfImpute(aveGPA~., data=CSEdataAveGPA, iter=6)

borutaCSE <- Boruta(aveGPA~., data = CSEdataAveGPA, doTrace=2, maxRuns=50 ) #dotrace =2 #maxruns = 50 more chance of importance
print(borutaCSE)


plot(borutaCSE, las=2, cex.axis=0.7)
plotImpHistory(borutaCSE)
getNonRejectedFormula(borutaCSE)
getConfirmedFormula(borutaCSE)

#tentative 
borCSE <- TentativeRoughFix(borutaCSE)
print(borCSE)
attStats(borutaCSE) # show normHits == 1 is confirmed 

#data partition

library(caret)
# Performs stratified random split of the data set
TrainingIndex <- createDataPartition(CSEdataAveGPA$aveGPA, p=0.6, list = FALSE)
aveGPATrainingSet <- CSEdataAveGPA[TrainingIndex,] # Training Set
aveGPATestingSet <- CSEdataAveGPA[-TrainingIndex,] # Test Set


#random forest model

set.seed(123)
RFMCSE <- randomForest(aveGPA~., data = aveGPATrainingSet)# get all factors
RFMCSE
RFMCSE11 <- randomForest(aveGPA ~ GENDER + ETHNICITY + AGE_AT_ENTRY + NATION + FIRST_FALL_PELL_AMOUNT + 
                              FIRST_FALL_BRIGHT_FUTURES_AMOUNT + FIRST_FALL_NEED_BASED_LOANS_AMOUNT + 
                              FIRST_FALL_NON_NEED_BASED_LOANS_AMOUNT + AP_CREDITS + APPLICANT_TIER + 
                              HOURS_BROUGHT_TO_UNIVERSITY, data = aveGPATrainingSet, proximity=TRUE) # get nonrejectedfacotrs
RFMCSE11

# using performanceanalysis
model <- train(aveGPA~., data=CSEdataAveGPA, method="rf", TuneLength=3,trControl=trainControl(method = "cv",number=10,classProbs = TRUE))
pred <- predict(model, CSEdataAveGPA, "prob")
head(pred,10)

RFMCSE7 <- randomForest(aveGPA ~ GENDER + FIRST_FALL_BRIGHT_FUTURES_AMOUNT + FIRST_FALL_NEED_BASED_LOANS_AMOUNT + 
                          FIRST_FALL_NON_NEED_BASED_LOANS_AMOUNT + AP_CREDITS + APPLICANT_TIER + 
                          HOURS_BROUGHT_TO_UNIVERSITY, data=aveGPATrainingSet)
RFMCSE7
#prediction and confusion matrix
p <- predict(RFMCSE, aveGPATrainingSet)
confusionMatrix(p, aveGPATrainingSet$aveGPA)#0.9455
pt <- predict(RFMCSE, aveGPATestingSet)
confusionMatrix(pt, aveGPATestingSet$aveGPA)#0.6974


p11 <- predict(RFMCSE11, aveGPATrainingSet)
confusionMatrix(p11, aveGPATrainingSet$aveGPA)#0.9357
importance(RFMCSE11)
varImpPlot(RFMCSE11)
p11t <- predict(RFMCSE11, aveGPATestingSet)
confusionMatrix(p11t, aveGPATestingSet$aveGPA)#0.6974 

p7 <- predict(RFMCSE7, aveGPATrainingSet)
confusionMatrix(p7, aveGPATrainingSet$aveGPA)#0.8107 
p7t <- predict(RFMCSE7, aveGPATestingSet)
confusionMatrix(p7t, aveGPATestingSet$aveGPA)#0.7044

######################
## Multicollinearity###
######################



library(ggplot2)
library(cowplot)

#plot err
oob.error.RFMCSE11 <- data.frame(
  Trees=rep(1:nrow(RFMCSE11$err.rate), times=3),
  Type=rep(c("OOB","above2.77","below2.77"),each=nrow(RFMCSE11$err.rate)),
  Error=c(RFMCSE11$err.rate[,"OOB"],
          RFMCSE11$err.rate[,"above2.77"],
          RFMCSE11$err.rate[,"below2.77"])
)

ggplot(data=oob.error.RFMCSE11, aes(x=Trees, y=Error))+
  geom_line(aes(color=Type)) # shows that 500trees are enough


oob.values <- vector(length=10)
for(i in 1:10) {
  temp.model <- randomForest(aveGPA ~ ., data=aveGPATrainingSet, mtry=i, ntree=500)
  oob.values[i] <- RFMCSE11$err.rate[nrow(RFMCSE11$err.rate),1]
}
oob.values
## find the minimum error
min(oob.values)
## find the optimal value for mtry...
which(oob.values == min(oob.values))
## create a model for proximities using the best value for mtry
model <- randomForest(aveGPA ~ ., 
                      data=aveGPATrainingSet,
                      ntree=500, 
                      proximity=TRUE, 
                      mtry=which(oob.values == min(oob.values)))

## Now let's create an MDS-plot to show how the samples are related to each 
## other.
##
## Start by converting the proximity matrix into a distance matrix.
distance.matrix <- as.dist(1-model$proximity)

mds.stuff <- cmdscale(distance.matrix, eig=TRUE, x.ret=TRUE)

## calculate the percentage of variation that each MDS axis accounts for...
mds.var.per <- round(mds.stuff$eig/sum(mds.stuff$eig)*100, 1)

## now make a fancy looking plot that shows the MDS axes and the variation:
mds.values <- mds.stuff$points
mds.data <- data.frame(Sample=rownames(mds.values),
                       X=mds.values[,1],
                       Y=mds.values[,2],
                       aveGPA=aveGPATrainingSet$aveGPA)

ggplot(data=mds.data, aes(x=X, y=Y, label=Sample)) + 
  geom_text(aes(color=aveGPA)) +
  theme_bw() +
  xlab(paste("MDS1 - ", mds.var.per[1], "%", sep="")) +
  ylab(paste("MDS2 - ", mds.var.per[2], "%", sep="")) +
  ggtitle("MDS plot using (1 - Random Forest Proximities)")
# ggsave(file="random_forest_mds_plot.pdf")


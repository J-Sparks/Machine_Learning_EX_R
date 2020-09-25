#####################################
#### Clustering GPA < 2.0 FTIC ######
#####################################
write.csv( FTIC_1stTerm_ALL_DF,"FTIC_1stFallTermDF.csv")
library(readr)
FTIC_1stFallTermDF <- read_csv("FTIC_1stFallTermDF.csv", 
                               col_types = cols(X1 = col_skip()))
View(FTIC_1stFallTermDF)

# missing values
library(mice)
library(VIM)
p<- function(x){sum(is.na(x))/length(x)*100}
apply(New_FTIC_df, 2, p)

#choose cols for mice
New_FTIC_df <- FTIC_1stFallTermDF %>% 
  select(Stu_College,Stu_Department,PriorHrs,"UWFPriorHrs" = Stu_TotalInstHours,"UWFPriorTGP"=
         Stu_TotalInstGradePoints,Stu_CurrentTermLoad,Stu_GPAHighSchool,contains("ACT"),51:52 );glimpse(New_FTIC_df)

New_FTIC_df_test <- select(New_FTIC_df_test, -12);glimpse(New_FTIC_df_test)

#impute NAs
impute <- mice(New_FTIC_df_test[, 7:13], m=5, seed = 1234 ) 
NewHSGPA <- complete(impute, 1)
completeDFFTIC <- cbind(New_FTIC_df_test[ ,1:6,14], NewHSGPA);glimpse(completeDFFTIC)
apply(completeDFFTIC, 2, p)

#trim data for majors  unique majors

plot(completeDFFTIC[,-1])
###Scatter plot
plot(GPA1stFall~UWFPriorHrs, completeDFFTIC)
with(completeDFFTIC, text(GPA1stFall~UWFPriorHrs, labels=Stu_Department, pos=4,cex=.4))
# SCALE DATA ----
FTIC_trimScaled <- scale(completeDFFTIC[, -1])

# K-MEANS CLUSTERING ----
## CLUSTERING
fitK <- kmeans(FTIC_trimScaled[, -1], 3)
fitK
str(fitK)
fitK$cluster
plot(completeDFFTIC, col = fitK$cluster)

## CHOOSING K
k <- list()
for(i in 1:10){
  k[[i]] <- kmeans(FTIC_trimScaled, i)
}

k

betweenss_totss <- list()
for(i in 1:10){
  betweenss_totss[[i]] <- k[[i]]$betweenss/k[[i]]$totss
}

plot(1:10, betweenss_totss, type = "b", 
     ylab = "Between SS / Total SS", xlab = "Clusters (k)")

for(i in 1:4){
  plot(completeDFFTIC, col = k[[i]]$cluster)
}

# HIERACHICAL CLUSTERING ----
d <- dist(FTIC_trimScaled)
fitH <- hclust(d, "ward.D2")
plot(fitH) 
rect.hclust(fitH, k = 3, border = "red") 
clusters <- cutree(fitH, k = 3) 
plot(completeDFFTIC, col = clusters)

# MODEL-BASED CLUSTERING ----
library(mclust)
fitM <- Mclust(FTIC_trimScaled)
fitM
plot(fitM)


# DENSITY-BASED CLUSTERING ----
install.packages("dbscan")
library(dbscan)
kNNdistplot(FTIC_trimScaled, k = 10)
abline(h = 0.7, col = "red", lty = 2)
fitD <- dbscan(FTIC_trimScaled, eps = 0.7, minPts = 5)
fitD
plot(FTIC_trim[,-1], col = fitD$cluster)


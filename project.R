setwd("D:/Education/GradSchool/Courses/coursera/Parctical Machine Learning/")

# Download and load data
url_training<- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
url_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
# download.file(url=url_training, method='curl')
# download.file(url=url_test)
trainingRaw<- read.csv('pml-training.csv', header=T)
testingRaw <- read.csv('pml-testing.csv', header=T)

# Load libraries
library(caret)
library(ggplot2)
library(rattle)
library(rpart)
library(randomForest)

# PREPROCESS

# Create test and training from training data:
inTrain <- createDataPartition(y=trainingRaw$classe, p=0.6, list=FALSE)
training<- trainingRaw[inTrain, ]
testing <- trainingRaw[-inTrain, ]

# Remove attributes with >50% NA's
clmSize <- dim(training)[1]
rmAttr <- c()
for (clm in 1:length(training)){
    if (sum(is.na(training[,clm]))> 0){
        rmAttr <- c(rmAttr,clm)
    }
}
trainingPP <- training[,-rmAttr]

# Remove near-zero attributes
nzv <- nearZeroVar(trainingPP,saveMetrics = TRUE) 
trainingPP <- trainingPP[,-which(nzv$nzv==TRUE)]

# Remove ID(X) and user_name, do the same transformation on test data
trainingPP <- trainingPP[-c(1,2)]
testingPP  <- subset(testing, select=names(trainingPP))

# Remove correlated 'numeric' attributes
# typesAttr <- sapply(names(trainingPP),function (x) class(trainingPP[[x]]))
# corAttr <- cor(trainingPP[,which(typesAttr=='numeric')])

# Train with trainControl
#modFit <- train(classe~., data=trainingPP, method='rpart') is slow and inaccurate, not sure why
modFit1 <- rpart(classe ~ ., data=trainingPP)
modFit2 <- randomForest(classe~., data=trainingPP,prox=T, type='class')

# Estimate errors
pred1 <- predict(modFit1, newdata=testingPP[-length(testingPP)], type='class')
pred2 <- predict(modFit2, newdata=testingPP[-length(testingPP)], type='class')
confusionMatrix(pred1, testingPP$classe)
confusionMatrix(pred2, testingPP$classe)


## submission
newTest <- subset(testingRaw, select=names(trainingPP[-length(trainingPP)]))
pred3 <- predict(modFit1,newdata=newTest)
a<-sapply(names(newTest),function(x) class(newTest[[x]]))
b<-sapply(names(testingPP[-length(testingPP)]),function(x) class(testingPP[-length(testingPP)][[x]]))
inds <- which(a!=b)
for (i in inds)
    newTest[,i]<-as.numeric(newTest[,i])


pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pred3 <- c("B", "A", "A", "A", "A", "E", "D", "B", "A", "A", "B", "C", "B", "A", "E", "E", "A", "B", "B", "B")
setwd("D:/Education/GradSchool/Courses/coursera/Parctical Machine Learning/autoSubmit_files")
pml_write_files(pred3)
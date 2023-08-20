setwd("C:/Users/snoop/OneDrive/Documents/CourseraSpecScienceDonnees/8. Practical machine learning/week4/project")
install.packages(c("rlang", "tibble", "vctrs", "dplyr"))
library(rlang)
library(tibble)
library(vctrs)
library(dplyr)
install.packages("recipes")
library(recipes)
install.packages("caret")
library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
library(corrplot)
library(gbm)

set.seed(3333)

train<- read.csv("pml-training.csv",sep=",",stringsAsFactors = TRUE)
test <- read.csv("pml-testing.csv",sep=",",stringsAsFactors = TRUE)
dim(train)

nacol <- colnames(train)[colSums(is.na(train)) > 0]
train<- train[, !(colnames(test) %in% nacol)]
test <- test[, !(colnames(test) %in% nacol)]
dim(train)

remove <- nearZeroVar(train)
train <- train[, -remove]
test  <- test[, -remove]
dim(train)
dim(test)

train <- train[, -c(1:6)]
test <- test[, -c(1:6)]
dim(train)

cor_mat <- cor(train[, -53])
corrplot(cor_mat, order = "FPC", method = "color", type = "upper", 
         tl.cex = 0.8, tl.col = rgb(0, 0, 0))
highlyCorrelated = findCorrelation(cor_mat, cutoff=0.75)
names(train)[highlyCorrelated]

letsplit <- createDataPartition(train$classe, p = 0.8, list = FALSE)
training <- train[letsplit, ]
testing<- train[-letsplit, ]

## classification trees
decisionTreeMod1 <- rpart(classe ~ ., data=training, method="class")
fancyRpartPlot(decisionTreeMod1)
predictTreeMod1 <- predict(decisionTreeMod1, testData, type = "class")
cmtree <- confusionMatrix(predictTreeMod1, testData$classe)
cmtree
  # accuracy 0.7 is too low 

## random forest
controlRF <- trainControl(method="cv", number=3, verboseIter=FALSE)
modRF1 <- train(classe ~ ., data=training, method="rf", trControl=controlRF)
modRF1$finalModel
predictRF1 <- predict(modRF1, newdata=testing)
cmrf <- confusionMatrix(predictRF1, testData$classe)
cmrf 
  # overfitting

# General boosted model
GBM <- trainControl(method = "repeatedcv", number = 5, repeats = 1)
modGBM  <- train(classe ~ ., data=training, method = "gbm", trControl = GBM, verbose = FALSE)
modGBM$finalModel

print(modGBM)

predictGBM <- predict(modGBM, newdata=testing)
predictGBM <- as.factor(predictGBM)
levels(testing$classe)
levels(predictGBM)
cmGBM <- confusionMatrix(predictGBM, testing$classe)
cmGBM

Results <- predict(modGBM, newdata=test)
Results

#install.packages("caret")
library(ellipse)
library(caret)
library(tidyverse)
library(ggplot2)
library(caTools)
ins


iris <- read.csv("data/raw/Iris/iris.data", header=FALSE)
colnames(iris) <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species")

split <- sample.split(iris, SplitRatio =0.8)

train <- subset(iris, split==TRUE) #120 instancias
test <- subset(iris, split==FALSE) #30 instancias

x <- iris[,1:4]
y <- iris[,5] #variable a predecir (NOMBRE)


# boxplot para cada destribución
par(mfrow=c(1,4))
for(i in 1:4) {
    boxplot(x[,i], main=names(iris)[i])
}


#scales <- list(x=list(relation="free"), y=list(relation="free"))
#featurePlot(x=x, y=y, plot="density", scales=scales)

control <- trainControl(method="cv", number=10)
metric <- "Accuracy"


#MODELS
    
# a) linear algorithms
set.seed(7)
fit.lda <- train(Species~., data=iris, method="lda", metric=metric, trControl=control)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(Species~., data=iris, method="rpart", metric=metric, trControl=control)
# kNN
set.seed(7)
fit.knn <- train(Species~., data=iris, method="knn", metric=metric, trControl=control)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(Species~., data=iris, method="svmRadial", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(Species~., data=dataset, method="rf", metric=metric, trControl=control)


# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

dotplot(results)

print(fit.lda)



# estimate skill of LDA on the validation dataset
predictions <- predict(fit.lda, test)
confusionMatrix(predictions, test$Species)

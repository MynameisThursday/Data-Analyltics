##########################################
### Principal Component Analysis (PCA) ###
##########################################

library(ggfortify)
library(e1071)
library(class)
library(psych)

# PCA with iris dataset
wine <- (wine.data)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)

wine <- wine[,-c(4,5,10)]

pairs.panels(wine[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine$Type],pch=21)

wine.X <- wine[,2:11]
wine.X

principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)

summary(principal_components)

plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings


# linear model off of largest contributors to 1st PC

nflavphen <- wine$`Nonflavanoid Phenols`
flav <- wine$Flavanoids
totalphen <- wine$`Total phenols`
type <- wine$Type

#turn into knn
wine.new <- wine[,c(1,5,6,7)]
n = nrow(wine.new)
train.indexes <- sample(n,n*.7)
wine.train <-wine.new[train.indexes,]
wine.test <-wine.new[-train.indexes,]
sqrt(124) 
k=11
library("class")
KNNpred <- knn(train = wine.train[2:4], test = wine.test[2:4], cl = wine.train$Type, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = wine.test$Type, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(wine.test$Type)
accuracy <- c()
kr <- seq(1,5,1)

for (k in kr) {
  
  KNNpred <- knn(train = wine.train[2:4], test = wine.test[2:4], cl = wine.train$Type, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = wine.test$Type, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(wine.test$Type)) 
  
}

plot(kr,accuracy,type = "b")

#this was for attributes that contribute most to the 1st PC

#Knn for the full 13 variables

n2 = nrow(wine)
train.indexes <- sample(n2,n2*.7)
wine2.train <-wine[train.indexes,]
wine2.test <-wine[-train.indexes,]
sqrt(124) 
k=11
library("class")
KNNpred <- knn(train = wine2.train[2:11], test = wine2.test[2:11], cl = wine2.train$Type, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = wine2.test$Type, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(wine2.test$Type)
accuracy <- c()
kr <- seq(1,20,1)

for (k in kr) {
  
  KNNpred <- knn(train = wine2.train[2:11], test = wine2.test[2:11], cl = wine2.train$Type, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = wine2.test$Type, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(wine2.test$Type)) 
  
}

plot(kr,accuracy,type = "b")

#Drop the variables least contributing to the 1st PC and rerun PCA.

wine.drop <- wine[,-c(2,4,8)]
pairs.panels(wine.drop[,-1],gap = 0,bg = c("red", "yellow", "blue")[wine.drop$Type],pch=21)

wine.X <- wine.drop[,2:8]
wine.X

principal_components <- princomp(wine.X, cor = TRUE, score = TRUE)

summary(principal_components)

plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = wine.drop, colour = 'Type',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings

#knn from new pca

wine.drop.new <- wine.drop[,c(1,2,5,8)]
n = nrow(wine.drop.new)
train.indexes <- sample(n,n*.7)
wine.train <-wine.drop.new[train.indexes,]
wine.test <-wine.drop.new[-train.indexes,]
sqrt(124) 
k=11
library("class")
KNNpred <- knn(train = wine.train[2:4], test = wine.test[2:4], cl = wine.train$Type, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = wine.test$Type, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(wine.test$Type)
accuracy <- c()
kr <- seq(1,5,1)

for (k in kr) {
  
  KNNpred <- knn(train = wine.train[2:4], test = wine.test[2:4], cl = wine.train$Type, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = wine.test$Type, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(wine.test$Type)) 
  
}

plot(kr,accuracy,type = "b")

##Comparison of the 3 classification models 
" The model created from droping attributes that did not aline with the 1st PC was more accurate than the model with all attributes included which was more accurate than the model created from a pca showing which top three attributes allined the best with the 1st PC"
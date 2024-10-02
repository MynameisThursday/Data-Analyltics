"NB for abalone Data set use this as bluprint for iris"
library("e1071")
library("ggplot2")
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
abalone$age.group <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$age.group <- as.factor(abalone$age.group)
abalone <- abalone[,-c(1,9)]
classifier<-naiveBayes(abalone[,1:7], abalone[,8])
prediction <- predict(classifier, abalone[,1:7])
contingency.table <- table(prediction, abalone[,8], dnn=list('predicted','actual')) 
print(contingency.table)
parameters <- classifier$tables$whole_weight
m1 <- parameters["young",1][[1]]
m2 <- parameters["adult",1][[1]]
m3 <- parameters["old",1][[1]]

sd1 <- parameters["young",2][[1]]
sd2 <- parameters["adult",2][[1]]
sd3 <- parameters["old",2][[1]]

plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Petal length distribution for the 3 different species") 

curve(dnorm(x, m2, sd2), add=TRUE, col="blue") 
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green")

contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone[,8])

"Attempt at 3 (2 more) different subsets of features for NB Abalone"

abalone$weightclass <- cut(abalone$whole_weight, br=c(0,.5,1.5,4), labels = c("light", 'middle', 'heavy'))
abalone$weightclass <- as.factor(abalone$weightclass)
abalone <- abalone[,-c(8)]
classifier2<-naiveBayes(abalone[,1:7], abalone[,8])
prediction <- predict(classifier2, abalone[,1:7])
contingency.table <- table(prediction, abalone[,8], dnn=list('predicted','actual'))
print(contingency.table)
parameters2 <- classifier2$tables$whole_weight

m1 <- parameters2["light",1][[1]]
m2 <- parameters2["middle",1][[1]]
m3 <- parameters2["heavy",1][[1]]

sd1 <- parameters2["light",2][[1]]
sd2 <- parameters2["middle",2][[1]]
sd3 <- parameters2["heavy",2][[1]]


plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Weight Class distribution for the 3 different species") 

curve(dnorm(x, m2, sd2), add=TRUE, col="blue") 
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green")


contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone[,8])

####################################
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
abalone$calciumcarbonate.load <- cut(abalone$shell_weight, br=c(0,.2,.65,1), labels = c("low", 'mid', 'high'))
abalone$calciumcarbonate.load <- as.factor(abalone$calciumcarbonate.load)
abalone <- abalone[,-c(1)]
classifier3<-naiveBayes(abalone[,1:8], abalone[,9])
prediction <- predict(classifier3, abalone[,1:8])
contingency.table <- table(prediction, abalone[,9], dnn=list('predicted','actual'))
print(contingency.table)
parameters3 <- classifier3$tables$whole_weight

m1 <- parameters3["low",1][[1]]
m2 <- parameters3["mid",1][[1]]
m3 <- parameters3["high",1][[1]]

sd1 <- parameters3["low",2][[1]]
sd2 <- parameters3["mid",2][[1]]
sd3 <- parameters3["high",2][[1]]


plot(function(x) dnorm(x, m1, sd1), 0, 3, col="red", main="Calcium Carbonate Load distribution for the 3 different species") 

curve(dnorm(x, m2, sd2), add=TRUE, col="blue") 
curve(dnorm(x, m3, sd3 ), add=TRUE, col = "green")


contingency.matrix = as.matrix(contingency.table)

sum(diag(contingency.matrix))/length(abalone[,9])

###################################################

"KNN for Abalone use this as blueprint"
colnames(abalone) <- c("sex", "length", 'diameter', 'height', 'whole_weight', 'shucked_wieght', 'viscera_wieght', 'shell_weight', 'rings' ) 
abalone$age.group <- cut(abalone$rings, br=c(-1,8,11,35), labels = c("young", 'adult', 'old'))
abalone$age.group <- as.factor(abalone$age.group)
abalone <- abalone[,-c(1,9)]
n = nrow(abalone)
train.indexes <- sample(n,n*.7)
abalone.train <-abalone[train.indexes,]
abalone.test <-abalone[-train.indexes,]
sqrt(2924)

k = 55
library("class")
KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(abalone.test$age.group)
accuracy <- c()
ks <- seq(5,105,10)

for (k in ks) {
  
  KNNpred <- knn(train = abalone.train[1:7], test = abalone.test[1:7], cl = abalone.train$age.group, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = abalone.test$age.group, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(abalone.test$age.group)) 
  
}

plot(ks,accuracy,type = "b")

##########################################

"Attempt at knn for Iris data set"
"Using petal width"
iris <- iris[,-c(1,6)]
n = nrow(iris)
train.indexes <- sample(n,n*.7)
iris.train <-iris[train.indexes,]
iris.test <-iris[-train.indexes,]
sqrt(105*.7)
k=9
library("class")
KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Petal.Width, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = iris.test$Petal.Width, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Petal.Width)

accuracy <- c()
kr <- seq(1,5,1)

for (k in kr) {
  
  KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Petal.Width, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Petal.Width, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Petal.Width)) 
  
}

plot(kr,accuracy,type = "b")

" this is for sepal length"
KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Sepal.Length, k = k)
contingency.table <- table(Actual=KNNpred, Predicted = iris.test$Sepal.Length, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(iris.test$Sepal.Length)

accuracy <- c()
kr <- seq(1,5,1)

for (k in kr) {
  
  KNNpred <- knn(train = iris.train[1:4], test = iris.test[1:4], cl = iris.train$Sepal.Length, k = k)
  cm = as.matrix(table(Actual=KNNpred, Predicted = iris.test$Sepal.Length, dnn=list('predicted','actual')))
  
  accuracy <- c(accuracy,sum(diag(cm))/length(iris.test$Sepal.Length)) 
  
}

plot(kr,accuracy,type = "b")
"This is K-Means (centroid stuff) for Abalone, use as blueprint"
ggplot(abalone, aes(x = length, y = whole_weight, colour = age.group)) +
  geom_point()
set.seed(123)
abalone.km <- kmeans(abalone[,-8], centers = 3)
abalone.km$tot.withinss
assigned.clusters <- as.factor(abalone.km$cluster)
ggplot(abalone, aes(x = length, y = whole_weight, colour = assigned.clusters)) +
  geom_point()
labeled.clusters <- as.character(assigned.clusters)
labeled.clusters[labeled.clusters==1] <- "young"
labeled.clusters[labeled.clusters==2] <- "old"
labeled.clusters[labeled.clusters==3] <- "adult"
table(labeled.clusters, abalone$age.group, dnn=list('predicted','actual'))
wcss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
  
  abalone.km <- kmeans(abalone[,-8], centers = k)
  
  wcss <- c(wcss,abalone.km$tot.withinss)
  
}

plot(ks,wcss,type = "b")

################################
"this is k-means for Iris"
iris <- iris[,-c(1)]
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, colour = Species)) +
  geom_point()
set.seed(123)
iris.km <- kmeans(iris[,-5], centers = 3)
iris.km$tot.withinss
assigned.clusters2 <- as.factor(iris.km$cluster)
ggplot(iris, aes(x = Petal.Width, y = Petal.Length, colour = assigned.clusters2)) +
  geom_point()
labeled.clusters <- as.character(assigned.clusters2)
labeled.clusters[labeled.clusters==1] <- "setosa"
labeled.clusters[labeled.clusters==2] <- "versicolor"
labeled.clusters[labeled.clusters==3] <- "virginica"
table(labeled.clusters, iris$Species, dnn=list('predicted','actual'))
wcss <- c()
ks <- c(2,3,4,5)
for (k in ks) {
iris.km <- kmeans(iris[,-5], centers = k)

wcss <- c(wcss,iris.km$tot.withinss)

}

plot(ks,wcss,type = "b")
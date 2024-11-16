# lab 5
wine <- (wine.data)
names(wine) <- c("Type","Alcohol","Malic acid","Ash","Alcalinity of ash","Magnesium","Total phenols","Flavanoids","Nonflavanoid Phenols","Proanthocyanins","Color Intensity","Hue","Od280/od315 of diluted wines","Proline")
head(wine)

wine$Type <- as.factor(wine$Type)
wine.new <- wine[,c(1,2,3,4)]

library("caret")
library(e1071)

## take copy
dataset <- wine.new

# ## split train/test
train.indexes <- sample(150,0.7*150)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## separate x (features) & y (class labels)
x <- dataset[,2:4] 
y <- dataset[,1]

## feature boxplots
boxplot(x, main="wine features")

## class label distributions
plot(y)

## feature-class plots
featurePlot(x=x, y=y, plot="ellipse")

featurePlot(x=x, y=y, plot="box")

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

ggplot(dataset, aes(x = Alcohol, y = `Malic acid`, colour = Type)) +
  geom_point()


## train SVM model - linear kernel
train$Type <- as.factor(train$Type)
svm.mod0 <- svm(Type ~ ., data = train, kernel = 'linear')

svm.mod0

train.pred <- predict(svm.mod0, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


## train SVM model - polynomial kernel
svm.mod1 <- svm(Type ~ ., data = train, kernel = 'polynomial')

svm.mod1

train.pred <- predict(svm.mod1, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


## Tuned SVM - polynomial
tuned.svm <- tune.svm(Type~., data = train, kernel = 'polynomial',gamma = seq(1/2^nrow(wine.new),1, .01), cost = 2^seq(-6, 4, 2))
tuned.svm

svm.mod2 <- svm(Type ~ ., data = train, kernel = 'polynomial', gamma = 0.69, cost = .25)

svm.mod2

train.pred <- predict(svm.mod2, train)

cm = as.matrix(table(Actual = train$Type, Predicted = train.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


### Test set prediction ###

## model 0
test.pred <- predict(svm.mod0, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## model 1
test.pred <- predict(svm.mod1, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

## model 2
test.pred <- predict(svm.mod2, test)

cm = as.matrix(table(Actual = test$Type, Predicted = test.pred))

cm

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)


##########################################

# knn time!

wine.new2 <- wine[,c(1,2,3,4)]
n = nrow(wine.new2)
train.indexes <- sample(n,n*.7)
wine.train <-wine.new2[train.indexes,]
wine.test <-wine.new2[-train.indexes,]
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

# compare these later

#################################

#NY data set

library("caret")
library(e1071)

## take copy
dataset <- `NY.House.Dataset.(2)`
dataset <- dataset[,c(3,6)]


# ## split train/test
train.indexes <- sample(150,0.7*150)

train <- dataset[train.indexes,]
test <- dataset[-train.indexes,]

## separate x (features) & y (class labels)
x2 <- dataset[,2] 
y2 <- dataset[,1]


ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()


svm.mod0 <- svm(log10(PRICE) ~ log10(PROPERTYSQFT), data = train, kernel = 'linear')

svm.mod0

train.pred <- predict(svm.mod0, train)

ggplot(train, aes(x = train.pred, y = log10(PRICE))) +
  geom_point()

###
#linear model

PROPERTYSQFT <- `NY.House.Dataset.(2)`$PROPERTYSQFT
PRICE <- `NY.House.Dataset.(2)`$PRICE

lin.mod.NY <- lm(log10(PRICE)~log10(PROPERTYSQFT), data=`NY.House.Dataset.(2)`)
plot(lin.mod.NY)

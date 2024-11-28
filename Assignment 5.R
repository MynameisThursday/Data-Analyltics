library(readr)
library(ggplot2)
library(e1071)
library(caret)
library("class")

dataset <- NYC_Citywide_Annualized_Calendar_Sales_Update_20241116
dataset.Manha <- dataset[dataset$BOROUGH== 1,]
dataset.Manha <- dataset.Manha[complete.cases(dataset.Manha[15:20]),]
dataset.Manha <- dataset.Manha[,c(2,20,15,16)]
dataset.Manha <- dataset.Manha[which(dataset.Manha$LAND.SQUARE.FEET>1000 & dataset.Manha$GROSS.SQUARE.FEET>1000 & dataset.Manha$SALE.PRICE>1000 & dataset.Manha$LAND.SQUARE.FEET<10^5 & dataset.Manha$GROSS.SQUARE.FEET<10^5 & dataset.Manha$SALE.PRICE<10^8 & dataset.Manha$SALE.PRICE>10^2),]



summary(dataset.Manha$SALE.PRICE)
summary(dataset.Manha$LAND.SQUARE.FEET)
summary(dataset.Manha$GROSS.SQUARE.FEET)
SALE.PRICE. <- as.numeric(dataset.Manha$SALE.PRICE)
LAND.SQUARE.FEET. <- as.numeric(dataset.Manha$LAND.SQUARE.FEET)
GROSS.SQUARE.FEET. <- as.numeric(dataset.Manha$GROSS.SQUARE.FEET)
boxplot(SALE.PRICE.,LAND.SQUARE.FEET.,GROSS.SQUARE.FEET.)
hist(SALE.PRICE.,prob=TRUE)
hist(LAND.SQUARE.FEET.,prob=TRUE)
hist(GROSS.SQUARE.FEET.,prob=TRUE)


ggplot(dataset.Manha, aes(x = LAND.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point()

ggplot(dataset.Manha, aes(x = GROSS.SQUARE.FEET, y = SALE.PRICE)) +
  geom_point()


### Gross square ft, and land square ft has a linear relationship with sale price

lin.mod.NY <- lm(SALE.PRICE~(LAND.SQUARE.FEET+GROSS.SQUARE.FEET), data=dataset.Manha)
plot(lin.mod.NY)

summary(lin.mod.NY)

dataset.nFLATIRON <- dataset.Manha[dataset.Manha$NEIGHBORHOOD!= "FLATIRON",]
dataset.nUPPER <-dataset.Manha[dataset.Manha$NEIGHBORHOOD!= "UPPER EAST SIDE (59-79)",]

lin.mod.nFLATIRON <- lm(SALE.PRICE~(LAND.SQUARE.FEET+GROSS.SQUARE.FEET), data=dataset.nFLATIRON)
plot(lin.mod.nFLATIRON)

summary(lin.mod.nFLATIRON)

lin.mod.nUPPER <- lm(SALE.PRICE~(LAND.SQUARE.FEET+GROSS.SQUARE.FEET), data=dataset.nUPPER)
plot(lin.mod.nUPPER)

summary(lin.mod.nUPPER)
## D. supervised models

## naive bayes for tax class
dataset.NB <- dataset[dataset$BOROUGH== 1,]
dataset.NB <- dataset.NB[c(4,12,13,14,15,16,17)]
classifier<-naiveBayes(dataset.NB[,2:7], dataset.NB[,1])
prediction <- predict(classifier, dataset.NB[,2:7])
contingency.table <- table(prediction, dataset.NB[,1], dnn=list('predicted','actual')) 
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(dataset.NB[,1])



dataset.NB <- dataset.NB[complete.cases(dataset.NB[1:7]),]
## train SVM

library(ggfortify)

# PCA with iris dataset
dataset.df <- dataset.NB
head(dataset.df)

# creating another dataframe from iris dataset that contains the columns from 2 to 6
dataset.X <- dataset.df[,2:4]
dataset.X

principal_components <- princomp(dataset.X, cor = TRUE, score = TRUE)

summary(principal_components)

# using the plot() function, we can plot the principal components.
plot(principal_components)

# plotting the principal_components using the a line in plot() functions 
plot(principal_components, type = "l")

# using rhw biplot() function we can plot the components
biplot(principal_components)

## using autoplot() function to plot the components
autoplot(principal_components, data = dataset.NB, colour = 'TAX.CLASS.AS.OF.FINAL.ROLL',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

# loadings
principal_components$loadings

#Predicting sale price from square feet for different data set

dataset.2 <- dataset[dataset$BOROUGH== 2,]
dataset.2 <- as.data.frame(dataset.2)
dataset.2 <- dataset.2[complete.cases(dataset.2[15:20]),]
dataset.2 <- dataset.2[,c(2,20,15,16)]
dataset.2 <- dataset.2[which(dataset.2$LAND.SQUARE.FEET>1000 & dataset.2$GROSS.SQUARE.FEET>1000 & dataset.2$SALE.PRICE>1000 & dataset.2$LAND.SQUARE.FEET<10^5 & dataset.2$GROSS.SQUARE.FEET<10^5 & dataset.2$SALE.PRICE<10^8 & dataset.2$SALE.PRICE>10^2),]

lin.mod.NY2 <- lm(SALE.PRICE~(LAND.SQUARE.FEET+GROSS.SQUARE.FEET), data=dataset.2)
plot(lin.mod.NY2)

summary(lin.mod.NY2)

#naive Bayes for tax class for other dataset

dataset.NB2 <- dataset[dataset$BOROUGH== 2,]
dataset.NB2 <- dataset.NB2[c(4,12,13,14,15,16,17)]
classifier<-naiveBayes(dataset.NB2[,2:7], dataset.NB2[,1])
prediction <- predict(classifier, dataset.NB2[,2:7])
contingency.table <- table(prediction, dataset.NB2[,1], dnn=list('predicted','actual')) 
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(dataset.NB2[,1])


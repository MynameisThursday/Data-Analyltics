######################################
"Q1 Histograms"
"for 20<spi<50 "
na.indexes <- is.na(epi2024results_DA_F24_lab03$SPI)
epi.subset <- epi2024results_DA_F24_lab03[!na.indexes,]
summary(epi.subset$SPI)

epi.subset <- epi.subset[epi.subset$SPI>=20 & epi.subset$SPI<=50, c("country","SPI")]

hist(epi.subset$SPI)
hist(epi.subset$SPI, seq(0., 100., 5.0), prob=TRUE)
rug(epi.subset$SPI)
SPI <- epi.subset$SPI
lines(density(SPI,na.rm=TRUE,bw=3))
lines(density(SPI,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)

plot(qqnorm(SPI))
qqline(SPI)

plot(qqnorm(SPI[which(SPI<=58)]))
qqline(SPI[which(SPI<=58)])

"for 50<SPI<100"
na.indexes <- is.na(epi2024results_DA_F24_lab03$SPI)
epi.subset <- epi2024results_DA_F24_lab03[!na.indexes,]
summary(epi.subset$SPI)

epi.subset <- epi.subset[epi.subset$SPI>=50 & epi.subset$SPI<=100, c("country","SPI")]

hist(epi.subset$SPI)
hist(epi.subset$SPI, seq(0., 100., 5.0), prob=TRUE)
rug(epi.subset$SPI)
SPI <- epi.subset$SPI
lines(density(SPI,na.rm=TRUE,bw=3))
lines(density(SPI,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)

plot(qqnorm(SPI))
qqline(SPI)

plot(qqnorm(SPI[which(SPI<=58)]))
qqline(SPI[which(SPI<=58)])

############
"Q2"
"linear models"
epi.region <- epi2024results_DA_F24_lab03[epi2024results_DA_F24_lab03$region=="Sub-Saharan Africa", c("SPI","MKP","MHP","MPE","PAR")]

na.indexes <- is.na(epi2024results_DA_F24_lab03$SPI)
epi.subset <- epi2024results_DA_F24_lab03[!na.indexes,]
summary(epi.subset$SPI)

na.indexes2 <- is.na(epi2024results_DA_F24_lab03$MKP)
epi.subset2 <- epi2024results_DA_F24_lab03[!na.indexes2,]
summary(epi.subset2$MKP)

na.indexes3 <- is.na(epi2024results_DA_F24_lab03$MHP)
epi.subset3 <- epi2024results_DA_F24_lab03[!na.indexes3,]
summary(epi.subset3$MHP)

na.indexes4 <- is.na(epi2024results_DA_F24_lab03$MPE)
epi.subset4 <- epi2024results_DA_F24_lab03[!na.indexes4,]
summary(epi.subset4$MPE)

na.indexes5 <- is.na(epi2024results_DA_F24_lab03$PAR)
epi.subset5 <- epi2024results_DA_F24_lab03[!na.indexes5,]
summary(epi.subset5$PAR)

EPI <- epi.region$EPI>=50 & epi.region$EPI<=100
SPI <- epi.region$SPI>=50 & epi.region$SPI<=100
MKP <- epi.region$MKP>=50 & epi.region$MKP<=100
MHP <- epi.region$MHP>=50 & epi.region$MHP<=100
MPE <- epi.region$MPE>=50 & epi.region$MPE<=100
PAR <- epi.region$PAR>=50 & epi.region$PAR<=100
 
lin.mod.epi <- lm(EPI~SPI+MKP+MHP+MPE+PAR, data=epi2024results_DA_F24_lab03)
plot(lin.mod.epi)
summary(lin.mod.epi)


################

"Questions for office hours about this"

SPI <- epi2024results_DA_F24_lab03$SPI
MKP <- epi2024results_DA_F24_lab03$MKP
MHP <- epi2024results_DA_F24_lab03$MHP
MPE <- epi2024results_DA_F24_lab03$MPE
PAR <- epi2024results_DA_F24_lab03$PAR
EPI <- epi2024results_DA_F24_lab03$EPI


lin.mod.epi2 <- lm(EPI~SPI+MKP+MHP+MPE+PAR, data=epi2024results_DA_F24_lab03, na.rm=TRUE)
plot(lin.mod.epi2)
summary(lin.mod.epi2)
"Summary explanation: SInce the Adjusted R^2 values for both the linear model fitted to a subset of variables from the region of subsaharan afirca and the model for the whole data set are the same, neither model is more indicative of predicting EPI values. Moreover, the low R^2 value of about .45 indicates that the 5 variables chosen are not indicative of predicting EPI values."



########################
"Q3"
"KNN"
library("class")
library("e1071")
library("ggplot2")
epi2024results <- epi2024results_DA_F24_lab03[,c(5,6,7,8,14,15)]
epi.ASG <- epi2024results[epi2024results$region %in% c("Sub-Saharan Africa","Asia-Pacific","Global West"),]

n = nrow(epi.ASG)
train.indexes <- sample(n,n*.7)
epi.train <-epi.ASG[train.indexes,]
epi.test <-epi.ASG[-train.indexes,]
sqrt(65)
k = 8
epi.KNNpred <- knn(train = epi.train[2:6], test = epi.test[2:6], cl = epi.train$region, k = k)

contingency.table <- table(Actual=epi.KNNpred, Predicted = epi.test$region, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(epi.test$region)



epi.EFL <- epi2024results[epi2024results$region %in% c("Eastern Europe","Former Soviet States","Latin America & Caribbean"),]
n = nrow(epi.EFL)
train.indexes <- sample(n,n*.7)
epi.train <-epi.EFL[train.indexes,]
epi.test <-epi.EFL[-train.indexes,]
sqrt(44)
k = 7
epi.KNNpred <- knn(train = epi.train[2:6], test = epi.test[2:6], cl = epi.train$region, k = k)

contingency.table <- table(Actual=epi.KNNpred, Predicted = epi.test$region, dnn=list('predicted','actual'))
print(contingency.table)
contingency.matrix = as.matrix(contingency.table)
sum(diag(contingency.matrix))/length(epi.test$region)

"I created models that would predict from given variables, regions from either my first regional subset (containing Asia-Pacific, Sub-Saharan Africa, and the Global West) or my second subset (containing Eastern Europe, the Former Soviet States, and Latin America & Caribbean). The model to predict countries from the first data set performed better than the second as indicated by the summations of the contigency matrices shown for each."

###########################
"Q4"
"K-means"

ggplot(epi.ASG, aes(x = EPI, y = BDH, colour = region)) +
  geom_point()
set.seed(123)
epi.ASG.km <- kmeans(epi.ASG[,-1], centers = 3)
epi.ASG.km$tot.withinss
assigned.clusters <- as.factor(epi.ASG.km$cluster)
ggplot(epi.ASG, aes(x = EPI, y = BDH, colour = assigned.clusters)) +
  geom_point()
wcss <- c()
ks <- c(2,3,4,5,6)

for (k in ks) {
  epi.ASG.km <- kmeans(epi.ASG[,-1], centers = k)
  wcss <- c(wss,epi.ASG.km$tot.withinss)
}

plot(ks,wcss,type = "b")


ggplot(epi.EFL, aes(x = EPI, y = BDH, colour = region)) +
  geom_point()
set.seed(123)
epi.EFL.km <- kmeans(epi.EFL[,-1], centers = 3)
epi.EFL.km$tot.withinss
assigned.clusters <- as.factor(epi.EFL.km$cluster)
ggplot(epi.EFL, aes(x = EPI, y = BDH, colour = assigned.clusters)) +
  geom_point()
wcss <- c()
kr <- c(2,3,4,5,6)

for (k in kr) {
  epi.EFL.km <- kmeans(epi.EFL[,-1], centers = k)
  wcss <- c(wss,epi.EFL.km$tot.withinss)
}

plot(kr,wcss,type = "b")

"The model to predict countries in the second subset (containing Eastern Europe, the Former Soviet States, and Latin America & Caribbean) was a better model than the former as its change in wcss is smaller over given kr "
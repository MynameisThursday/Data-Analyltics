covid1 <- us.counties.2022
covid2 <- us.counties.2023
attach(covid1)
attach(covid2)

normalize <- function(x) {return (log(x))}

covid1[5:6] <- as.data.frame(lapply(covid1[5:6], normalize))

covid2[5:6] <- as.data.frame(lapply(covid2[5:6], normalize))

"drop na's and inf's"
covid1 <- covid1[complete.cases(covid1[5:6]),]
covid1 <- covid1[covid1$deaths!= Inf & covid1$deaths!= -Inf & covid1$cases!= Inf & covid1$cases!= -Inf,]

covid2 <- covid2[complete.cases(covid2[5:6]),]
covid2 <- covid2[covid2$deaths!= Inf & covid2$deaths!= -Inf & covid2$cases!= Inf & covid2$cases!= -Inf,]


cases2023 <- covid2$cases
cases2022 <- covid1$cases
deaths2023 <- covid2$deaths
deaths2022 <- covid1$deaths

summary(cases2022)
summary(cases2023)
summary(deaths2022)
summary(deaths2023)

boxplot(cases2022,cases2023)
boxplot(deaths2022,deaths2023)

hist(cases2022,prob=TRUE)
lines(density(cases2022,na.rm=TRUE,bw=.5))

hist(cases2023,prob=TRUE)
lines(density(cases2023,na.rm=TRUE,bw=.5))

hist(deaths2022,prob=TRUE)
lines(density(deaths2022,na.rm=TRUE,bw=.5))


hist(deaths2023,prob=TRUE)
lines(density(deaths2023,na.rm=TRUE,bw=.5))



"1a. So, according to the summary stats (specifically the 1st and 3rd quartiles along with the Maximums) it seems as though cases rose in number from 2022 to 2023. The same can be said moreover for deaths occuring from 2022 to 2023 as the same statistics from before show a decrease in such values."

"1b in all 4 variables (deaths and cases for 2022 and 2023) it seems that a normal distribution fits the histograms the best. The means of both (cases) variables seem to be centered around 9.9=log(x) while the deaths seem to be centered around 4.6-7=log(x) for each variable." 

plot(ecdf(cases2022), do.points=FALSE)
plot(ecdf(rnorm(1000, 8.914, 1)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(cases2022))

plot(ecdf(cases2023), do.points=FALSE)
plot(ecdf(rnorm(1000, 9.068, .9)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(cases2023))

plot(ecdf(deaths2022), do.points=FALSE)
plot(ecdf(rnorm(1000, 4.606, .8)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(deaths2022))

plot(ecdf(deaths2023), do.points=FALSE)
plot(ecdf(rnorm(1000, 4.745, .8)), do.points=FALSE) # ecdf of normal distr with mean=45, sd= 10
lines(ecdf(deaths2023))

"1a. So, according to the summary stats (specifically the 1st and 3rd quartiles along with the Maximums) it seems as though cases rose in number from 2022 to 2023. The same can be said moreover for deaths occuring from 2022 to 2023 as the same statistics from before show a decrease in such values."

"1b in all 4 variables (deaths and cases for 2022 and 2023) it seems that a normal distribution fits the histograms the best. The means of both (cases) variables seem to be centered around 9.9=log(x) while the deaths seem to be centered around 4.6-7=log(x) for each variable." 

"1c. "

#########################################

"q2"

NYhousefactors <- NY.House.Dataset[NY.House.Dataset$STATE=="Bayside, NY 11360", c("BEDS","BATH","PROPERTYSQFT","PRICE")]
summary(NYhousefactors$BEDS)
summary(NYhousefactors$BATH)
summary(NYhousefactors$PROPERTYSQFT)

BEDS <- NY.House.Dataset$BEDS
BATH <- NY.House.Dataset$BATH
PROPERTYSQFT <- NY.House.Dataset$PROPERTYSQFT
PRICE <- NY.House.Dataset$PRICE

lin.mod.NY <- lm(PRICE~BEDS+BATH+PROPERTYSQFT, data=NY.House.Dataset)
plot(lin.mod.NY)




BEDS <- NYhousefactors$BEDS
BATH <- NYhousefactors$BATH
PROPERTYSQFT <- NYhousefactors$PROPERTYSQFT
PRICE <- NYhousefactors$PRICE

lin.mod.NY2 <- lm(PRICE~BEDS+BATH+PROPERTYSQFT, data=NYhousefactors, na.rm=TRUE)
plot(lin.mod.NY2)



summary(lin.mod.NY)
summary(lin.mod.NY2)

"2a. For the linear model (baths) have the largest coefficeint (in terms of absolute value) as determined by summary stats, thus they are the biggest determinant of housing price in this model. This is due to the factor of beds having a larger effect on the models trend then beds or property per square feet."
"2b. I subsetted the data set by geographic location specifically looking at Bayside, NY 11360. In this subset we see that baths have much more of an effect on the linear model than beds as opposed to the model made from the whole data set. This can be seen from the baths coefficient estimate being larger than the beds for the subset model, but not for the whole data set model.   "
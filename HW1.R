#Setting working directory
getwd()
setwd("E:/BA with R/HW1")
getwd()
set.seed(4)
library(lmtest)
#Execise 1
#Importing CSV file
mydata <- read.csv("mortality.csv",header=TRUE)

#checking imopted data
head(mydata)

#Making Identifier column as Null
mydata$City <- NULL
sapply(mydata, mode)

#Performing simple linear regression
#Removing NA values
mydata <- na.omit(mydata)

test1 <- lm(Mortality ~ . , data = mydata)
hist(test1$residuals,freq=FALSE)
lines(density(test1$residuals), col = "blue")

#Heteroscadasticity test
plot(test1) ##  Checking only residuals vs fitted plot and Q-Q plot
# Checking null hypothesis
bptest(test1) 

#Identify outliers via studentized residuals
plot(predict(test1), rstudent(test1))
identify(predict(test1), rstudent(test1))
mydata <- mydata[-36,]
View(mydata)

# Checking if residuals are normally distributed
test2 <- lm(Mortality ~ . , data = mydata)
hist(test2$residuals,freq=FALSE)
lines(density(test2$residuals), col = "blue")

#Performing  linear Regression
reg1 <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg1)


#Stepwise regression
reg1 <- lm(Mortality ~ JanTemp + JulyTemp + RelHum + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg1)

reg2 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain + Education + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg2)

reg3 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC + pop + HHSiz + income + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg3)

reg4 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC  + HHSiz + income + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg4)

reg5 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC  + HHSiz  + HCPot + NOxPot +S02Pot, data=mydata)
summary(reg5)

reg6 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC  + HHSiz  + HCPot +S02Pot, data=mydata)
summary(reg6)

reg7 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC  + HCPot +S02Pot, data=mydata)
summary(reg7)

reg8 <- lm(Mortality ~ JanTemp + JulyTemp  + Rain  + PopDensity + NW + WC +S02Pot, data=mydata)
summary(reg8)

reg9 <- lm(Mortality ~ JanTemp + Rain  + PopDensity + NW + WC +S02Pot, data=mydata)
summary(reg9)

#Perform forward regression
null <- lm(Mortality ~ 1, data = mydata)
null
full <- lm(Mortality ~ ., data = mydata)
full
selectedmodel <- step(null, scope=list(lower=null, upper=full), direction="forward")
selectedmodel
forward <- lm(formula = Mortality ~ NW + JanTemp + S02Pot + Rain + WC + 
                PopDensity + JulyTemp, data = mydata)
summary(forward)


#Principal Component Analysis
pcadata <- read.csv("mortality.csv",header=TRUE)


pcadata$Mortality <- NULL
pcadata$City <- NULL

pcadata <- na.omit(pcadata)


fit <- princomp(pcadata, cor=TRUE)

summary(fit)

loadings(fit)

fit$scores

plot(fit, type="lines")

pcadata1<- cbind(pcadata1, fit$scores)
View(pcadata1)

model <- lm(Mortality ~ Comp.1 + Comp.2 + Comp.3 + Comp.4 + Comp.5 , data=pcadata1)
summary(model)

model <- lm(Mortality ~ Comp.1 + Comp.2 + Comp.3 + Comp.5 , data=pcadata1)
summary(model)

model <- lm(Mortality ~ Comp.1 + Comp.2 + Comp.3 + Comp.4  , data=pcadata1)
summary(model)

model <- lm(Mortality ~ Comp.1 + Comp.2 + Comp.3   , data=pcadata1)
summary(model)


#Assosiation Rule
library(arules)
library(Matrix)

set.seed(4)
setwd("E:/BA with R/HW1")
trans <- read.transactions("transactions.csv", format = "single", sep = ",", cols = c("Transaction", "Product"), rm.duplicates = FALSE)
summary(trans)

itemFrequencyPlot(trans,topN=20,type="absolute")

rules <- apriori(trans, parameter = list(supp = 0.03, conf = 0.20, minlen = 2))
rules <- sort(rules, by="lift", decreasing=TRUE)


inspect(rules)
summary(rules)

# Remove duplicate rules
redundant_index <- is.redundant(rules)
print(redundant_index)
pruned_rules <- rules[!redundant_index]
inspect(pruned_rules)
summary(pruned_rules)




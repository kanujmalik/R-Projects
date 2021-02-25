#Load libraries
library(effects)
library(car)

#Load the titanic data
titanic <- read.table("titanic.txt", header = T)
str(titanic)
dim(titanic)
names(titanic)
summary(titanic$Age)


#Convert PClass and Sex to factors
titanic$PClass = factor(titanic$PClass)
titanic$Sex = factor(titanic$Sex)
titanic$Survived <- as.factor(titanic$Survived)


#remove cases with missing data
titanic_new <- na.omit(titanic)
dim(titanic_new)
str(titanic_new)


#Exploring the data
#Class
summary(titanic_new$PClass)
plot(titanic_new$PClass, main="Passenger Class", xlab="Class", ylab="count")

#Sex
summary(titanic_new$Sex)
plot(titanic_new$Sex, main="Sex", xlab="Sex", ylab = "count")

#Age
summary(titanic_new$Age)
hist(titanic_new$Age, main = "Age", xlab = "Age", ylab = "Count")

#Survived
survived <- table(titanic_new$Survived)
barplot(survived, main = "Survived passengers", xlab = "Survived/Not Survived", ylab = "Count", names.arg = c("Not survived", "Survived"))

#Survived - PClass
sur_class <- table(titanic_new$Survived, titanic_new$PClass)
plot(sur_class, main="Survived vs Class", xlab = "Survived", ylab = "Class")

#Survived-Sex
sur_sex <- table(titanic_new$Survived, titanic_new$Sex)
plot(sur_sex, main="Survived vs Sex", xlab = "Survived", ylab = "Sex")

#Main effects model
survival <- titanic_new[,5]
xtitanic <- titanic_new[,c(2,3,4)]
attach(titanic_new)

titanic_model1 <- glm(survival~., family = binomial, data = xtitanic)
summary(titanic_model1)
A<-allEffects(titanic_model1)
plot(A)


#Significance
Anova(titanic_model1)
1-pchisq(695.14,751)

#Diagnostics - Check fit and predictivity
lp <- titanic_model1$linear.predictors
coeff = coef(titanic_model1)
fv = fitted.values(titanic_model1)
expb=exp(coeff)
cbind(expb,coeff)

#non-linear effect of age by fitting a quadratic in age
titanic_model2 <- glm(survival~.+I(Age^2), family = binomial, data = xtitanic)
summary(titanic_model2)
B<-allEffects(titanic_model2)
plot(B)
Anova(titanic_model2)
1-pchisq(693.07,750)

#Interaction between the three explanatory variables
Inter<- step(titanic_model2, scope = ~.^2)
summary(Inter)
C<-allEffects(Inter)
plot(C[1])
plot(C[2])
plot(C[3])
plot(C[4])
Anova(Inter)
1-pchisq(631,744)
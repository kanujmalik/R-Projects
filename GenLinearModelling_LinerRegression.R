#1.	Using simple linear regression examine the relationship between
#distance to the trail and house prices in 2014. 
#Report on whether distance is significantly related to house prices
#and quantify any relationship you find.

#Read the data into houseprice
houseprices <- read.csv("houseprices.csv", header=TRUE)
attach(houseprices)
head(houseprices)

# plot the data on an x-y graph
plot(distance,price2014, xlab="DIstance to trail", ylab="House prices")

#Set up the linear regression
house.regression <- lm(price2014~distance, data = houseprices)
summary(house.regression)

#Add the regression line to the plot
abline(house.regression,col="blue")


#------------------------------------------------------------------------


#2.	Carry out a multiple linear regression including all other covariates
#( apart from housenum)  aswell as distance as explanatory variables
#on the response variables of price2014.  
#Some of these will need to be declared as factors.. 
#Report on the estimated regression coefficients and the percentage of variance
#explained.

#Do multiple regression including all other covariates
house.mulregression <- lm(price2014~distance + acre + bedgroup + bikescore
                          + garage_spaces + no_full_baths + no_half_baths
                          + no_rooms + squarefeet + walkscore + zip, data = houseprices)


summary(house.mulregression)


#--------------------------------------------------------------------------------

#3. Carry out a backwards elimination from this model, 
#removing terms one at a time until all non-significant terms have been removed
#from the model. You should use Anova() in library car at each step to determine
#the least important variable to remove at each step.
#Report on the final model. Is distance still significant? 
#What is the effect of each of the remaining variables in the model on house prices?


#Backward elimination with a single step:
newmodel <- step(house.mulregression, direction = "backward")
newmodel

#Backward elimintation one step at a time using anova():
#removing bedgroup
reg_Bedgroup <- lm(price2014~distance + acre + bikescore
                          + garage_spaces + no_full_baths + no_half_baths
                          + no_rooms + squarefeet + walkscore + zip, data = houseprices)

anova(house.mulregression, reg_Bedgroup)

#removing acre
reg_acre <- lm(price2014~distance + bikescore
                   + garage_spaces + no_full_baths + no_half_baths
                   + no_rooms + squarefeet + walkscore + zip, data = houseprices)

anova(reg_Bedgroup,reg_acre)


# removing distance
reg_distance <- lm(price2014~bikescore
               + garage_spaces + no_full_baths + no_half_baths
               + no_rooms + squarefeet + walkscore + zip, data = houseprices)

anova(reg_acre,reg_distance)


#removing no_rooms
reg_norooms <- lm(price2014~bikescore
                  + garage_spaces + no_full_baths + no_half_baths
                   + squarefeet + walkscore + zip, data = houseprices)

anova(reg_distance, reg_norooms)

#removing no_half_baths
reg_nohalfbaths <- lm(price2014~bikescore
                      + garage_spaces + no_full_baths
                      + squarefeet + walkscore + zip, data = houseprices)

anova(reg_norooms,reg_nohalfbaths)


#plot graph of house prices with final variables
plot(bikescore,price2014, xlab="Bike score", ylab="House prices")
plot(garage_spaces,price2014, xlab="Bike score", ylab="House prices")
plot(no_full_baths,price2014, xlab="Full Baths", ylab="House prices")
plot(squarefeet,price2014, xlab="Square Feet", ylab="House prices")
plot(walkscore,price2014, xlab="Walk score", ylab="House prices")
plot(zip,price2014, xlab="Zip", ylab="House prices")



#-------------------------------------------------------------------------------


#4.	Finally, using the standardized residuals from the final model
#determine whether the residuals follow a normal distribution. 
#What is your conclusion?  Carry out any other diagnostic tests you feel
#are appropriate. and if necessary ,amend your model

hist(residuals(newmodel))
boxplot(residuals(newmodel))


#------------------------------------------------------------------------------
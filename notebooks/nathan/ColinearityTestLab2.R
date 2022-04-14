d <- Fifa2022Data

#install.packages("car")
#install.packages("GGally")

library(corrplot)
library(car)
library(GGally)

df <- d[!is.na(d$Value),]

# 1.1 Checking for Colinearity

# Using a correlation matrix is beneficial to seeing if there is a high correlation between 
# certain variables. We want to assess these to see if high correlation between variables will 
# create an issue for our analysis.

colintest <- df[,c("Value","Age","Height_cm","Weight_kg","Overall","International_Reputation","Potential")]
head(colintest)

cor(colintest)
corrplot(cor(colintest), method="pie")


# 1.2 Testing the VIF

# The Variance Inflation Factor is a way to assess how much the variance of an 
# estimated regression coefficient increases if your variables are correlated. 
# a VIF of 1 indicates no correlation, but over 1 indicates that the variable could have
# some correlation to other factors. A VIF of over 5 is a value that would be of concern
# when considering running a regression. 

model <- lm(Value ~ Age + Height_cm + Weight_kg + Overall + International_Reputation, data = colintest)
vif(model)


#1.3 Multicolinearity Matrix
colmatrix <- cbind(colintest,df["Preferred_Foot"])
#Nationality has too many categories, only 15 different are allowed for this function


ggpairs(colmatrix)


# 2.1 Checking for Colinearity Log10(Value)















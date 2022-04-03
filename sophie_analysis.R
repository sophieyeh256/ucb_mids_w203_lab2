library(tidyverse)
library(magrittr)
library(ggplot2)
library(patchwork)
library(sandwich)
library(lmtest)
library(fec16)
library(stargazer)
library(nnet)

data <- read.csv('data/external/FIFA22_official_data.csv')
df <- (filter(data, Best.Position == "GK"))

na.omit(df)

money_formatter <- function(x) {
  x <- gsub("€", "", x)
  x <- case_when(
    grepl("K", x, fixed=TRUE) ~ as.numeric(gsub("K", "", x))*1e3,
    grepl("M", x, fixed=TRUE) ~  as.numeric(gsub("M", "", x))*1e6,
    TRUE ~ as.numeric(x)
  )
  return(x)
}
weight_formatter <- function(x) {
  x <- case_when(
    grepl("kg", x, fixed=TRUE) ~ as.numeric(gsub("kg", "", x))*1e3,
    grepl("cm", x, fixed=TRUE) ~  as.numeric(gsub("cm", "", x))*1e6,
    TRUE ~ as.numeric(x)
  )
  return(x)
}

df$Value <- money_formatter(df$Value)
df$Wage <- money_formatter(df$Wage)
df$Height <- weight_formatter(df$Height)
df$Weight <- weight_formatter(df$Weight)

# long_variables <- c(Age, 
                    # Nationality, 
                    # Overall, 
                    # Potential, 
                    # Club, 
                    # Wage, 
                    # Special, 
                    # Preferred.Foot,
                    # International.Reputation,
                    # Weak.Foot,
                    # Skill.Moves,
                    # Work.Rate,
                    # Body.Type,
                    # Height,
                    # Weight,
                    # Best.Overall.Rating
                    # )
  
  
model1 <- lm(Value ~ Age+ 
                         # Nationality+ # Costa Rica, Algeria
                         Overall+ # significant
                         Potential+ # significant
                         # Club+ # Djurgårdens IF, Cruzeiro, Crystal Palace, and others
                         Wage+
                         # Special+
                         Preferred.Foot+ # Right
                         International.Reputation+
                         # Weak.Foot+
                         # Skill.Moves+ # NA
                         # Work.Rate+
                         # Body.Type+ #Body.TypeUnique
                         Height+ # not significant
                         Weight, # not significant
                         # Best.Overall.Rating, 
                 data=df)
summary(model1)
glimpse(df)
model2 <- lm(Value ~ Age+ 
               Nationality+
               Overall+ 
               Potential+
               Club+ # Djurgårdens IF, Cruzeiro, Crystal Palace, and others
               # Wage+ 
               # Special+
               Preferred.Foot+ 
               International.Reputation+
               # Weak.Foot+
               # Skill.Moves+ 
               # Work.Rate+
               Body.Type+ #Body.TypeUnique
               Height+ 
               Weight, 
             # Best.Overall.Rating, 
             data=df)
summary(model2)

model3 <- multinom(Overall ~ Age+ 
               Nationality+ 
               Club+ 
               # Wage+ 
               # Special+
               Preferred.Foot+ 
               International.Reputation+
               Weak.Foot+
               Skill.Moves+
               Work.Rate+
               Body.Type+
               Height+ 
               Weight,
             # Best.Overall.Rating, 
             data=df)
output3 <- summary(model3)
print(output3)
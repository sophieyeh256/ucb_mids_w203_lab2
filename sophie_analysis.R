library(tidyverse)
library(magrittr)
library(ggplot2)
library(patchwork)
library(sandwich)
library(lmtest)
library(fec16)
library(stargazer)


data <- read.csv('data/external/FIFA22_official_data.csv')
gk_data <- (filter(data, Best.Position == "GK"))



model_1 <- lm(value ~.)


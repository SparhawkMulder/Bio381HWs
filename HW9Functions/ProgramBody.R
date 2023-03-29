# Homework 9
# Main file
# 3/29/2023
# Sparhawk Mulder


# Load packages ---------------------------------------

library(tidyverse)
library(dplyr)
library(MASS)
library(ggplot2)


# Source Functions ------------------------------------


source("HW9Functions/Functions_fun.R")

source("HW9Functions/Function2_fun.R")

# Global Variables -----------------------------------

FullData <- "CleanedData.csv"


# Program Body ---------------------------------------

# Get Data
Data <- getData(FullData)

# Clean and Format Data
Clean <- cleanData(Data)

Men <- makeMen(Clean)
Women <- makeWomen(Clean)

# Get Parameters (normal)
MenPars <- getParams(Men$avg_rating)
WomenPars <- getParams(Women$avg_rating)

# Create a random similar normal distribution
randMen <- make.similiar.normal(MenPars)
randWomen <- make.similiar.normal(WomenPars)

# t-test the two groups
ttest <- t.test(randMen$randData, y = randWomen$randData, alternative="two.sided")
print(ttest)

# Create Overlayed Histograms
comparisonHist(randWomen,randMen,"ComparisonHist.png")



# Alternate ---------------------------------------------------------------------
# Include weight
# do analysis on weigh instead of attractiveness
# t-test weights by sex
# histogram for weights

# Include weight data
wClean <- cleanData_weight(Data)

# Then this is all the same, but for weight

wMen <- makeMen(wClean)
wWomen <- makeWomen(wClean)
# Get Parameters (normal)
wMenPars <- getParams(wMen$X_weight)
wWomenPars <- getParams(wWomen$X_weight)
# Create a random similar normal distribution
wrandMen <- make.similiar.normal(wMenPars)
wrandWomen <- make.similiar.normal(wWomenPars)

# t-test for weight by sex
wieght_ttest <- t.test(wrandMen$randData, y = wrandWomen$randData, alternative="two.sided")
print(ttest)

# comparison hist for weights
weights_hist <- comparisonHist(wrandWomen,wrandMen,"ComparisonHist_weight.png")
print(weights_hist)

# Homework 9
# Source functions page
# Sparhawk Mulder
# 3/29/2023

# Load packages -------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggplot2)



# FUNCTION: getData ############################################
# packages: none
# purpose: read a CSV
# input: CSV file address
# output: a dataframe
# 
getData <- function(filename = NULL) {
  if (is.null(filename)) {
    return("No CSV file input") # warning message
    
  } else {
    
    df <- read.csv("CleanedData.csv", sep=",")
    return(df)
  }
}


# FUNCTION: cleanData ############################################
# packages: tidyverse
# purpose: clean up this specific dataset to the minimum columns
# input: a dataframe
# output: a smaller dataframe
# 
cleanData <- function(df=NULL) {
  if (is.null(df)) {
    message("no dataframe input!")
    return(NULL) # warning message
  }
  
  df <- dplyr::select(df, photo, avg_rating, photo_gender) #select desired columns
  df <- unique(df) # we only want one instance of each photo
  df <- arrange(df, photo) # arrange in photo ID order
  
  message("trimmed to only photos, rating, and gender")
  return(df)
}


# FUNCTION: MakeMen ############################################
# packages: dplyr
# purpose: split the dataframe into two, by sex
# input: a dataframe
# output: a smaller dataframe
# 
makeMen <- function(df=NULL) {
  Men <- filter(df, photo_gender=="Male")
  
  return(Men)
}


# FUNCTION: MakeWomen ############################################
# packages: dplyr
# purpose: split the dataframe into two, by sex
# input: a dataframe
# output: a smaller dataframe
# 
makeWomen <- function(df=NULL) {
  Women <- filter(df, photo_gender=="Female")
  
  return(Women)
}



# FUNCTION: getParams ############################################
# packages: none
# purpose: get the parameters of a normal model for the dataset
# input: 
# output: 
# 
getParams <- function(column=NULL) {
  if (is.vector(column) == FALSE) { # error message if column vectorisn't input
    message("please input df$column")
  }
  params <- fitdistr(column,"normal")
  
  message("Hint: for Mean, use [name]$estimate['mean']")
  message("Hint: for SD, use [name]$estimate['sd']")
  
  return(params)
}


# FUNCTION: make.similiar.normal ############################################
# packages: tidyverse
# purpose: create a random normal distribution with input parameters
# input: parameters for a normal model
# output: a dataframe
# 
make.similiar.normal <- function(params=NULL,size=100) {
  
  randData <- rnorm(size, mean=params$estimate["mean"], sd=params$estimate["sd"]) #make data
  
  newdf <- data.frame(randData) %>% #make df

  return(newdf)
}


# FUNCTION: comparisonHist ##################################################
# packages: ggplot2
# purpose: 
# input: 
# output: 
# 
comparisonHist <- function(pinkdata=rnorm(100), bluedata=rnorm(100), outputName=NULL) {
  
  ComparisonHist <- ggplot(data=pinkdata, aes(x=randData)) + #could add x-axis name
    geom_histogram(fill="pink", color="black", alpha=0.60) +
    
    geom_histogram(data=bluedata,
                   fill="lightblue", color="black", alpha=0.5) +
    
  if(is.null(outputName)) {message("Hint: input a .png file name")}
  save(ComparisonHist, file=outputName)
  message("histogram saved!")
  return(ComparisonHist)
}

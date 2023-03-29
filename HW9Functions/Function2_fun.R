# Homework 9
# Alternate Source functions page
# Sparhawk Mulder
# 3/29/2023

# FUNCTION: cleanData_weight ############################################
# packages: tidyverse
# purpose: clean up this specific dataset to the minimum columns + weight
# input: a dataframe
# output: a smaller dataframe
# 
cleanData_weight <- function(df=NULL) {
  if (is.null(df)) {
    message("no dataframe input!")
    return(NULL) # warning message
  }
  
  df <- dplyr::select(df, photo, avg_rating, photo_gender, X_weight) #select desired columns
  df <- unique(df) # we only want one instance of each photo
  df <- arrange(df, photo) # arrange in photo ID order
  
  message("trimmed to only photos, rating, weight, and gender")
  return(df)
}
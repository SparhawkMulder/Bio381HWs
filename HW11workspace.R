# Homework 11 workspace script

# Libraries ----------------------------------------------

library(readr)
library(tidyverse)
library(dplyr)
library(MASS)


# Getting "multiple" datasets ----------------------------
# I'm going to make a model to make imitator data from 
# the beauty data I've been using for previous homeworks

Full <- read.csv("CleanedData.csv")

Small <- dplyr::select(Full, photo, avg_rating) %>%
  arrange(photo) %>%
  unique()

Pars <- fitdistr(Small$avg_rating,"normal")
ParsMean <- Pars$estimate["mean"]
ParsSD <- Pars$estimate["sd"]

RanFileLength <- c(75,100) # bounds of dataset lengths
Files_n <- 7 # number of files
FileFolder <- "HW11Datas/"


FileBuilder(file_n=Files_n,
            file_folder=FileFolder,
            mean=ParsMean,
            sd=ParsSD,
            file_size = RanFileLength)


# Batch Processing: Summary stats-------------------------------------------

FileNames <- list.files(path=FileFolder,
                        pattern=".csv")
Mean <- rep(NA,times=length(FileNames))
SD <- rep(NA,times=length(FileNames))


Stats <- data.frame(FileNames,Mean,SD)


for (i in 1:Files_n) {
  data <- read.table(file=paste(FileFolder,FileNames[i],sep=""),
                     sep=",",
                     header=TRUE)
  data <- data[complete.cases(data),]
  . <- fitdistr(data[,2], "Normal")
  Stats[i,2] <- .$estimate["mean"]
  Stats[i,3] <- .$estimate["sd"]
}
head(Stats)



# make the summary file

file_out <- "HW11SummaryStats.csv"

write.table(cat("# Summary stats for ",
                "batch processing of rating data","\n",
                "# timestamp: ",as.character(Sys.time()),"\n",
                "# Sparhawk Mulder","\n",
                "# ------------------------", "\n",
                "\n",
                file=file_out,
                row.names="",
                col.names="",
                sep=""))
# now add the data frame
write.table(x=Stats,
            file=file_out,
            row.names=FALSE,
            col.names=TRUE,
            sep=",",
            append=TRUE)

# Mini Histograms --------------------------------------------

par(mfrow=c(2,4))

for (i in seq_along(FileNames)) {
  data <- read.table(file=paste(FileFolder,FileNames[i],sep=""),
                     sep=",",
                     header=TRUE)
  hist(data$rating)
}




# Project Megazord -------------------------------------------

ID <- c()
avg_rating <- c()

for (i in 1:Files_n) {
  data <- read.table(file=paste(FileFolder,FileNames[i],sep=""),
                     sep=",",
                     header=TRUE) # read in next data file
  ID <- c(ID,data$photo)
  avg_rating <- c(avg_rating,data$rating)
}


Megazord <- data.frame(ID,avg_rating)

# Megazord Histogram

hist(Megazord$avg_rating) # I'm not focused on making this pretty, just showing I can



# Old test stuff: 

# ID <- seq_along(FileNames)
# file_name <- FileNames
# Megazord <- data.frame(ID,file_name)
#
# 
# for (i in seq_along(FileNames)) {
#   .<-read.csv(paste(FileFolder,FileNames[1],sep=""))
#   Megazord[file_name==FileNames[i],] <- .
# }
# 
# 










# FUNCTION: FileBuilder #########################
# packages: 
# purpose: create a random file following a given model
# input: file_n = number of files to create
#       : file_folder = name of folder for random files
#       : file_size = c(min,max) number of rows in file
#       : file_na = number on average of NA values per column
#       : mean = mean
#       : sd = sd
# output: a set of random normal distribution files
# 
FileBuilder <- function(file_n=30,
                        file_folder="HW11Datas/",
                        file_size=c(15,100),
                        file_na=3,
                        mean=NULL,
                        sd=NULL) {
  
  if(dir.exists(file_folder)) unlink(file_folder,
                                     recursive = TRUE) # clean out directory
  dir.create(file_folder) # make directory for the files
  
  for (i in seq_len(file_n)) {
    file_length <- sample(file_size[1]:file_size[2],size=1) # get number of rows
    photos <- seq(1:file_length) # create number of photos
    rating <- rnorm(file_length,mean=mean, sd=sd) # create ratings
    df <- data.frame(photos,rating) # bind into a data frame
    
    bad_vals <- rpois(n=1,lambda=file_na) # determine NA number
    df[sample(nrow(df),size=bad_vals),2] <- NA # random NA in var_y
    # still in for loop!
    
    # create label for file name with padded zeroes
    file_label <- paste(file_folder,
                        "ranFile",
                        formatC(i, # creates leading zeroes, for ease of alphabetizing
                                width=3,
                                format="d",
                                flag="0"),
                        ".csv",sep="")
    
    # set up data file and incorporate time stamp and minimal metadata
    write.table(cat("# Simulated random data file for batch processing","\n",
                    "# timestamp: ",as.character(Sys.time()),"\n",
                    "# Sparhawk Mulder","\n",
                    "# ------------------------", "\n",
                    "\n",
                    file=file_label, #callback
                    row.names="",
                    col.names="",
                    sep=""))
    
    # now add the data frame
    write.table(x=df,
                file=file_label,
                sep=",",
                row.names=FALSE,
                append=TRUE) # df doesn't overwrite metadata)
  }
}


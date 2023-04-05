# Homework 10 workspace
# Sparhawk

library(tidyverse)

# function to calculate number of zeroes in a vector

# FUNCTION: zeroCount #########################
# packages: none
# purpose: count the number of zeroes in a vector
# input: a numeric vector
# output: a single integer
# 
zeroCount <- function(x=runif(10)) {
  count <- 0
  for (i in seq_along(x)) {
    if (x[i]==0) {
      count <- count + 1
    }
  }
  return(count)
}

# testing
x <- c(0,0,3,7,4,40,0) # 5 zeros
print(zeroCount(x))

40 %% 10 == 0


# Now do it with subsetting

# FUNCTION: zeroCount2 #########################
# packages: none
# purpose: count the number of zeroes in a vector
# input: a numeric vector
# output: a single integer
# 
zeroCount2 <- function(x=runif(10)) {
  count <- length(x[x %% 10 == 0])
  return(count)
}

# testing
print(zeroCount2(x))




# Q3 ---------------------------------------

# Fxn reads row/column numbers and creates a matrix of their products

# FUNCTION: productMatrix #########################
# packages: none
# purpose: create a matrix of the products of the row and column numbers
# input: the row and column lengths of a matrix
# output: a matrix
# 
productMatrix <- function(rows=5, columns=4) {
  m <- matrix(data = NA,nrow=rows,ncol=columns)
  for (i in 1:rows) {
    for (j in 1:columns) {
      m[i,j] <- i*j # these would be a vector[i]*vector[j] if the inputs were vectors
    } 
  } 
  return(m)
}
# test
print(productMatrix())




# Q4 -------------------------------------------------



# Creating data frame

# NOT THIS:
# Treatment1 <- rnorm(100,mean = 10, sd = 2)
# Treatment2 <- rnorm(100,mean = 20, sd = 4)
# Control <- rnorm(100,mean = 7, sd = 3)
# 
# Data <- data.frame(Treatment1,Treatment2,Control)

groups <- c(rep("Control",100),rep("Treatment1",100),rep("Treatment2",100))
data <- c(rnorm(100,mean = 7, sd = 3),
          rnorm(100,mean = 10, sd = 2),
          rnorm(100,mean = 20, sd = 4))
John <- data.frame(groups,data)

print(John)

# Reshuffle and calc mean function

# FUNCTION: shuffle_and_means #########################
# packages: none
# purpose:  
# input: 
# output: 
# 
shuffle_and_means <- function(df=NULL) {
  if(is.null(df)) {stop("gimme a data frame or fugeddaboutit") }
  
  df[,2] <- sample(df[,2])
  
  means <- c(mean(df$data[1:100]),mean(df$data[101:200]),mean(df$data[201:300]))
  # wish I remembered how to select by another column's values
  
  return(means)
}
# test
print(shuffle_and_means(John))

     


# Good job bro now do that 100 more times!

# Data frame for means storage
meanDF <- matrix(data = NA, nrow = 100, ncol = 4, )
meanDF <- data.frame(meanDF)
meanDF <- rename(meanDF,replication=X1, ControlAvg=X2, Treat1Avg=X3, Treat2Avg=X4)

# for loop:

for (i in 1:100) {
  meanDF[i,] <- c(i,shuffle_and_means(John))
}





# Histograms -------------------------------------------

qplot(meanDF$ControlAvg)

ggplot(meanDF,mapping=aes(x=ControlAvg)) +
  geom_histogram(fill="black", color="black",alpha=0.2) +
  geom_histogram(aes(x=Treat1Avg), fill="blue",color="black",alpha=0.4) +
  geom_histogram(aes(x=Treat2Avg), fill="red",color="black",alpha=0.4)

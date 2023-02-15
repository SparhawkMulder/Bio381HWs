# Eigenvalue (lambda) where "Determinant(A-lambda*I) = 0"
 #   A is a matrix
 #   I is an identity matrix 
 #   1. run matrix arithmatic
 #   2. determinant
 #   3. solve resulting quadratic
 #   4.Run eigen value through arithmatic again to find eigen vector 

n_dims <- sample(seq(from=3,to=10),1)
typeof(n_dims)

vec1 <- seq(from=1,to=n_dims^2)

samp1 <- sample(vec1,length(vec1), replace=FALSE)

mat1 <- matrix(samp1,n_dims)

matTrans <- t(mat1)
print(matTrans)

sum1row <- sum(mat1[1,])
mean1row <- mean(mat1[1,])

sumLastrow <- sum(mat1[n_dims,])
meanLastrow <- mean(mat1[n_dims,])


eigen(mat1)
t(eigen(mat1)$vectors)
typeof(eigen(mat1)$values)
typeof(eigen(mat1)$vectors)


#----------------------------------------------------------------


my_matrix <- matrix(runif(16, min=-1, max=1),nrow=4, ncol=4)
print(my_matrix)

fuel <- runif(100,min=0,max=1)
octaneFuel <- fuel>0.5
my_logical <- matrix(octaneFuel,nrow=10,ncol=10)
print(my_logical)


my_letters <- sample(letters,size=length(letters),replace=FALSE)


my_list <- list(my_matrix[2,2], my_logical[1,2], my_letters[2])
typeof(my_list[[1]])
typeof(my_list[[2]])
typeof(my_list[[3]])
summary(my_list)

my_list2 <- c(my_matrix[2,2], my_logical[1,2], my_letters[2])
summary(my_list2)


#--------------------------------------------------------------------


my_unis <- runif(26,min=0,max=10)
my_letters <- sample(LETTERS,size=length(LETTERS), replace=FALSE)

my_frame <- data.frame(my_unis,my_letters)

my_frame[sample(seq(from=1,to=nrow(my_frame)),4),1] <- NA

which(!complete.cases(my_frame[,1]))


#reorder for col 2 to be in alphabetical order

alphabetical <- order(my_frame[,2])
typeof(alphabetical)

my_frame2 <- my_frame[alphabetical,2] #this gets me a vector, not a frame...

my_frame2 <- my_frame[alphabetical,]


mean(my_frame2[,1],na.rm = TRUE)

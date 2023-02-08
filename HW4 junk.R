# Atomic vector intro (HW)
x <- c(1, 2, 3, 4, 5)
x <- c("hello", "world")

# runif(n) generates n random deviants from a bell curve between 0 and 1
# advanced: runif(n, min = ?, max = ?)
rand <- runif(20, min = 0, max = 20)
print(rand)
# set.seed(#) makes the random number generaters sync (they're still random)


# rep() replicates the values in x. 
# rep.int(x, times) replicates that many times
# rep_length(x, length.out) replicates it out to a certain
rep(rand)


#seq() generates a random sequence
seq(5)



x <- 1.1
a <- 2.2
b <- 3.3
z <- x ^a^b
print(z)

z <- (x^a)^b
print(z)

z <- 3*x^3 + 2*x^2 + 1
print(z)



z <- seq(8)
v <- seq(from = 7, to = 1)
y <- c(z,v)
print(y)

z <- seq(5)
t <- seq(from = 1, to = 5)
rep(z, times = t)

z <- seq(from = 5, to = 1)
t <- seq(from = 1, to = 5)
rep(z, times = t)


set.seed(0)

z <- runif(2, min = -50, max = 50)
names(z) <- c("x val", "y val")
print(z)

r <- sqrt(z[1]^2 + z[2]^2)

polar_z <- c(r, asin(z[2]/r))

polar_z2 <- c(sqrt(z[1]^2 + z[2]^2), atan(z[2]/z[1]))



queue <- c("sheep", "fox", "owl", "ant")

queue <- append(queue, "serpent")

queue <- replace(queue, 1, NA)

queue <- queue[-1]

queue <- append(queue, "donkey", after = 0)

names(queue) <- queue
print(queue)
queue <- queue[names(queue) != "serpent"] # square brackets takes the subset

names(queue) <- queue
print(queue)
queue <- queue[names(queue) != "owl"]

position <- which(queue == "ant")
queue[3] <- "aphid"
queue <- append(queue, "ant", after = position)

position <- which(queue == "aphid")
print(position)



full <- seq(from = 1, to = 100)
div2 <- full%%2
odds <- rep(full, times = div2) #replicates only elements of full not divisible by 2

div3 <- odds%%3
#div3b <- replace(div3, 2, 1) # nah nevermind
nothrees <- rep(odds, times = div3) #this REPEATS some of the remaining values (the ones with remainders of more than 1)
nothrees <- unique(nothrees)

div7 <- nothrees%%7
final <- rep(nothrees, times = div7)
unique(final)

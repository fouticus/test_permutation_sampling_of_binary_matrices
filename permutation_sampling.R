# Author: Alex Fout
# Versions: 
# R: 4.0.2
# Matrix: 1.2-18
library(Matrix)

set.seed(utf8ToInt("@fouticus"))
M <- 1*matrix(runif(16)<0.2, 4, 4)
M <- as(M, "dgTMatrix")
M

# Sampled matrix isn't necessarily binary
M@i <- sample(M@i)
M@i <- sample(M@i)
M@i <- sample(M@i)
M

# Sampling isn't necessarily uniform
states <- new.env(hash=T)
M11 <- numeric(100000)
for(i in 1:100000){
  M@i <- sample(M@i)
  # record the matrix as a string in a hash table to count number of occurrences
  key <- paste(M, collapse=",")
  val <- states[[key]]
  if(is.null(val)){
    states[[key]] <- 1
  } else {
    states[[key]] <- val + 1
  }
  M11[i] <- M[1,1]   # record the 1, 1 spot
}
sapply(ls(states), function(x){print(states[[x]])})


# Marginal isn't the same as the binomial distribution
table(M11)/100000
dbinom(0:2, 2, 2/5)

# there's some sampling error here obviously, but the numbers are so far off, it should be obvious. 
# Plus, hopefully it's obvious that the empirical probabilities are nowhere near symmetric, as the binomial distribution always is.

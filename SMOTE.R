#' Over-sampling method to handle imbalanced classification data set
#' 
#' This function selects a subset of samples from the minor class, which are labeled 1,
#' to generate more samples from the minor class by using SMOTE method. 
#' Then the samples in the minor class and major class are balanced. 
#' 
#' @param X The design matrix, of dimensions n * p, without an intercept.
#' Each row is an observation vector.
#' @param Y The response vector of dimension n * 1.
#' Each row has binary value 0 or 1. The number of 0 is larger than the number of 1.
#' @param seed.val The seed used in the subsampling.
#'
SMOTE <- function(X, Y, seed.val) {
  ### Set seed
  set.seed(seed.val)
  
  ##############################################
  ### Get the dimension for the design matrix X.
  ##############################################
  n <- dim(X)[1]
  p <- dim(X)[2]
  
  ########################################################
  ### Get the index for the major class and minor class
  ########################################################
  class0.index <- which(Y == 0)
  class1.index <- which(Y == 1)
  
  n0 <- length(class0.index)
  n1 <- length(class1.index)
  
  if (n0 + n1 != n) print("Error! Double check the response")
  
  ########################################################
  ### Get the distance matrix
  ########################################################
  X.class1 <- X[class1.index, ]
  dist.matrix <- Euclidean.distance(X.class1)
  
  ########################################################
  ### Generate data by using SMOTE
  ########################################################
  X.generated <- matrix(0, n1, p)
  Y.generated <- rep(1, n1)
  
  for (i in 1 : n1) {
    sorted.dist <- sort(dist.matrix[i, ], index.return = TRUE)
    neighbor.index <- sorted.dist$ix[2 : 6]
    sel.neighbors <- sample(neighbor.index, 1, replace = FALSE)
    
    u <- runif(1)
    x.new <- X.class1[i, ] + u * (X.class1[sel.neighbors, ] - X.class1[i, ])
    X.generated[i, ] <- x.new
  }
  
  X.update <- rbind(X, X.generated)
  Y.update <- c(Y, Y.generated)
  
  outputs.list <- list()
  outputs.list$Xdata <- X.update
  outputs.list$Ydata <- Y.update
  
  return(outputs.list)
}



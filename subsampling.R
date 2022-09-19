#' Sub-sampling method to handle imbalanced classification data set
#' 
#' This function selects a subset of samples from the major class, which are labeled 0.
#' Then the samples in the minor class and major class are balanced. 
#' 
#' @param X The design matrix, of dimensions n * p, without an intercept.
#' Each row is an observation vector.
#' @param Y The response vector of dimension n * 1.
#' Each row has binary value 0 or 1. The number of 0 is larger than the number of 1.
#' @param ratio The ratio between major class and minor class, major = ratio * minor.
#' @param seed.val The seed used in the subsampling.
#'
subsampling <- function(X, Y, ratio, seed.val) {
  ### Set seed
  set.seed(seed.val)
  
  ########################
  ### Check the input
  ########################
  if(class(X)[1] != "matrix"){print("The input X must be numeric"); return(list())}
  if(class(Y) != "numeric"){print("The input response Y must be numeric"); return(list())}
  if(class(ratio) != "numeric"){print("The input ratio must be numeric"); return(list())}
  if(class(seed.val) != "numeric"){print("The input seed value must be numeric"); return(list())}
  
  ########################################################
  ### Get the dimension for the design matrix X.
  ########################################################
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
  
  #####################################################
  ### Sub-sampling the data sets
  #####################################################
  sub.class0.index <- sort(sample(class0.index, size = ratio * n1, replace = FALSE))
  
  X0 <- X[sub.class0.index, ]
  X1 <- X[class1.index, ]
  
  Y0 <- Y[sub.class0.index]
  Y1 <- Y[class1.index]
  
  X.subset <- rbind(X0, X1)
  Y.subset <- c(Y0, Y1)
  
  #######################
  ### Return outputs.list 
  #######################
  outputs.list <- list()
  outputs.list$Xdata <- X.subset
  outputs.list$Ydata <- Y.subset
  
  return(outputs.list)
}






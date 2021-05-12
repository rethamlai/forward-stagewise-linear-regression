# linRegFS function
linRegFS <- function(xtrain,ytrain,center,scale,maxIter,eps,figure) {
  
  # check for centering and scaling arguments
  if (center == TRUE) {
    if (scale == TRUE) {
      xtrainS <- scale(xtrain, scale=TRUE)
    } else {
      xtrainS <- scale(xtrain, scale=FALSE)
    }
  } else {
    xtrainS <- xtrain
  }
  
  # create placeholder matrixes and initialise to 0
  beta <- matrix(rep(0, length(ytrain)))
  beta_result <- matrix(0,nrow=length(ytrain),ncol=maxIter)
  
  # implement forward stagewise fitting
  for (j in 1:maxIter) {
    
    # compute gradient
    g = t(xtrainS) %*% (ytrain - (xtrainS %*% beta))
    # find the i-th element which maximises the absolute value of the gradient
    i <- match(max(abs(g)), abs(g))
    # save beta results
    beta_result[,j] = beta
    # update the i-th coefficient by eps * (sign of the gradient from i-th element)
    beta[i,1] <- beta[i,1] + (eps * sign(g[i,1]))
    
  }
  
  # plot results against L1 norm at each stagewise fit
  if (figure == T) {
    
    matplot(colSums(abs(beta_result)),t(beta_result),type="l",
            xlab=expression("||"*beta*"||"[1]),
            ylab=expression(beta),main="Boosting Forward Stagewise")
    
  }
  
  # return results
  rslt=list(beta=beta_result)
  return(rslt)
  
}
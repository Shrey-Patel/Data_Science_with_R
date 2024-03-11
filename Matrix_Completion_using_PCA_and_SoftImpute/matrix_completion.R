library(gamair)
library(softImpute)
library(tictoc)

# loading data set
data(chicago)
X <- chicago
head(X)

# checking for missing values
sapply(X, function(x) sum(is.na(x)))

# creating matrix with missing value
X1 <- na.omit(X)
X1 <- scale(X1)
dim(X1)
head(X1)
chi.omit <- 200                                       # nomit
set.seed (1234)
in.row <- sample ( 719, chi.omit)                     # ina
in.col <- sample (1:7, chi.omit , replace = TRUE)     # inb
X1.omit <- X1                                         # Xna
index.omit <- cbind (in.row , in.col)                 # index.na
X1.omit[index.omit] <- NA
# Your missing data matrix is then X1.omit

#head(X1.omit)
#head(X1)
#summary(X1.omit)
#summary(X1)

###### matrix completion using prcomp ######

fit.pca <- function(X , M) {
  pcob <- prcomp(X)
  with(pcob, 
       x[, 1:M, drop = FALSE] %*% 
         (t(rotation[, 1:M, drop = FALSE]))
  )
}

Mat.Complete <- function(X,thresh,maxiter){
  # Step 1 #
  # calculating Xhat
  Xhat <- X1.omit
  xbar <- colMeans(X1.omit , na.rm = TRUE)
  Xhat[index.omit] <- xbar[in.col]
  
  # Step 2 #
  # initializing progress variables
  rel_err <- 1                            # relative error
  iter <- 0                               # iterator
  ismiss <- is.na(X1.omit)
  mssold <- mean((scale(X1.omit, xbar, FALSE)[!ismiss])^2) # mse of non-missing elements (old version)
  mss0 <- mean(X1.omit[!ismiss]^2)        # mse of non-missing elements
  
  while(rel_err > thresh) {
    iter <- iter + 1 
    # Step 2(a)
    Xapp <- fit.pca(Xhat , M = 1)
    # Step 2(b)
    Xhat[ismiss] <- Xapp[ismiss] 
    # Step 2(c)
    mss <- mean(((X1.omit - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss)/mss0
    mssold <- mss
    cat("Iter:", iter, "MSS:", mss, "Rel. Err:", rel_err, "\n")
    if (iter >= maxiter){
      cat("WARNING: Maximum iterations reached. Exiting the loop.")
      break
    }
  }
  list <- list("x"=Xapp, "iter"=iter, "relerr"=rel_err)
  return(list)
}

tic("PCA")
output = Mat.Complete(X1.omit, 1e-7, 30)
toc()

###### matrix completion using softImpute ######

fit.sftimp <- function(X , M) {
  sftimp <- softImpute(X)
  with(sftimp,
       u[, 1:M, drop = FALSE] %*%
         (d[1:M] * t(v[, 1:M, drop = FALSE ]))
  )
}

Mat.Complete2 <- function(X,thresh,maxiter){
  # Step 1 #
  # calculating Xhat
  Xhat <- X1.omit
  xbar <- colMeans(X1.omit , na.rm = TRUE)
  Xhat[index.omit] <- xbar[in.col]
  
  # Step 2 #
  # initializing progress variables
  rel_err <- 1                            # relative error
  iter <- 0                               # iterator
  ismiss <- is.na(X1.omit)
  mssold <- mean((scale(X1.omit, xbar, FALSE)[!ismiss])^2) # mse of non-missing elements (old version)
  mss0 <- mean(X1.omit[!ismiss]^2)        # mse of non-missing elements
  
  while(rel_err > thresh) {
    iter <- iter + 1 
    # Step 2(a)
    Xapp <- fit.sftimp(Xhat , M = 1)
    # Step 2(b)
    Xhat[ismiss] <- Xapp[ismiss] 
    # Step 2(c)
    mss <- mean(((X1.omit - Xapp)[!ismiss])^2)
    rel_err <- (mssold - mss)/mss0
    mssold <- mss
    cat("Iter:", iter, "MSS:", mss, "Rel. Err:", rel_err, "\n")
    if (iter >= maxiter){
      cat("WARNING: Maximum iterations reached. Exiting the loop.")
      break
    }
  }
  list <- list("x"=Xapp, "iter"=iter, "relerr"=rel_err)
  return(list)
}

tic("softImpute")
output2 = Mat.Complete2(X1.omit, 1e-7, 30)
toc()

ismiss <- is.na(X1.omit)
(cor(output2$x[ismiss], X1[ismiss]))

cat("Correlation between imputed and actual values: ", 
      cor(output2$x[ismiss], X1[ismiss]))

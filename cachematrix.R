## This file is about a pair of functions that 
## can cache the inverse of a matrix

## This function creates a special "matrix" 
## objectthat can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize n as NULL;
  ## It will hold value of matrix inverse
  n <- NULL
  
  ## Method to set the matrix
  ## define the set function
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  
  ## Define the get fucntion
  get <- function() {
    ## returns value of the matrix argument
    x 
  }
  
  ## Assign value of n in parent environment
  setinverse <- function(inverse) {
    n <<- inverse
  }
  
  ## Get the inverse of the matrix
  getinverse <- function() {
    ## Return the inverse property
    n
  }
  
  ## Return a list of the methods
  ## you need this in order to refer
  ## to the functions with the $ operator
  list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



## This function computes the inverse of the 
## special"matrix" returned by makeCacheMatrix
## if the inverse hasalready been calculated 
## (and the matrix has not changed)
## then cacheSolve function should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getinverse()
  
  ## Return the inverse if its already set
  if( !is.null(n) ) {
    message("getting cached data")
    return(n)
  }
  ## Get the matrix from our object
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  ## n is the inverse of x matrix
  n <- solve(data)
  
  ## Set the inverse to the object
  x$setinverse(n)
  
  ## Return the matrix
  n
}

## Sample run:
##
## y <- matrix(rnorm(9,0,1),3,3)
## > y
##           [,1]       [,2]        [,3]
## [1,] -2.441683 -0.8858482 -0.66856748
## [2,] -1.210056 -2.0855634 -0.07738633
## [3,]  0.943955 -2.4794301 -1.37024604
## > z <- makeCacheMatrix(y)
## > cacheSolve(z)
##               [,1]          [,2]          [,3]
## [1,]  1.000000e+00 -5.551115e-17 -5.551115e-17
## [2,] -4.163336e-17  1.000000e+00  4.163336e-17
## [3,] -1.665335e-16 -2.220446e-16  1.000000e+00

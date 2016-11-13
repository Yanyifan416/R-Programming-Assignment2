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
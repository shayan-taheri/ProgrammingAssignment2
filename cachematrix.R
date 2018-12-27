## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function of "makeCacheMatrix" is used to create a special
## "Matrix" object to be used for caching its inverse version.

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  set <- function(y) {
    
    y <<- x
    
    inv <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  
}


## Write a short comment describing this function

## The following function is used to compute the inverse of
## the special matrix object returned by the "makeCacheMatrix" function.
## Correctly and stable calculation of the matrix inverse results in
## retrieval of the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    
    message("geting cached data")
    
    return(inv)
    
  }
  
  data <- x$get()
  
  inv <- solve(data, ...)
  
  x$setInverse(inv)
  
  inv
  
}

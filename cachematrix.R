## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates the cache of the inverse
## Call this if you need to change the inverse cache or create a new cache
## Function creates a list of four functions:
## set --> to change the matrix
## get --> get the matrix
## setInv --> set the inverse
## getInv --> get the inverse

makeCacheMatrix <- function(x = matrix()) {
    ## theinv is the inverse.  Nullify the inv
    theinv <- NULL
    ## The set function sets the matrix and initializes the
    ## matrix with input and the the inverse to NULL
    set <- function(y) {
        x <<- y
        theinv <<- NULL
    }
    ## get the input matrix
    get <- function () x
    ## set the inverse of the matrix to cache
    setInv <- function(inv) theinv <<- inv
    ## get the inverse of the matrix
    getInv <- function() theinv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## This function calculates the inverse of the matrix using the solve
## function.  If a cache already exists, returns the cache.  Call make
## cacheMatrix before calling this function for proper results.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## get the inverse from the cache if it is not null
    m <- x$getInv()
      if(!is.null(m)) {
	  message("getting cached data")
	  return(m)
      }
    ## Otherwise use the R solve function and set the cache
      data <- x$get()
      m <- solve(data)
      x$setInv(m)
      m
}

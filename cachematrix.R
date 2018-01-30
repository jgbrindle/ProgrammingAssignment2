## Solution to Programming Assignment 2 - Jeff Brindle.  R Programming
## Created 30 Jan 2018

## This code creates 2 functions to cache the result of a Matrix Inversion to use again if the matrix is unchanged.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly.

## This function creates a special "matrix" object that can cache its inverse.
## It creates a special matrix which is really a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(theInv) inv <<- theInv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the 
## inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

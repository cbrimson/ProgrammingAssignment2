## This file contatins two functions. 
## The first function creates an R object that stores a matrix and its inverse
## The second function calls on the first function for the inverse, and if it is null, calculates the inverse

## This function creates four functions and two data objects

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) inverse <<- solve
     getsolve <- function() inverse
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## cacheSolve calls on the inverse stored from makeCacheMatrix.
## If the inverse is found from makeCacheMatrix, the message will be printed and the inverse returned
## If the inverse is not found from makeCacheMatrix it will perform solve and print the inverse

cacheSolve <- function(x, ...) {
     inverse <- x$getsolve()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setsolve(inverse)
     inverse
}
#==================================================================================================
#= Name:        cacheMatrix
#= Created:     01-25-2015
#= Author:      Ben Holcombe
#= Description: makeCacheMatrix will accept a matrix variable and return a function where the
#               matrix supplied (x) will be embeded in its environment. This returned function 
#               will have the set, get, setsolve and getsolve functions inside it.
#==================================================================================================
#= Test:        
# Creates a 3x3 matrix generating a random number for each cell
# x <- matrix(runif(9, 5.0, 7.5), nrow=3, ncol=3)
# Populate mtx
# mtx <- makeCacheMatrix(x)
# Calculate the solve (inverse) of the matrix
# cacheSolve(mtx)
# Print the two matrices
# print(mtx$get())
# Inverted Matrix
# print(mtx$getsolve())
#==================================================================================================
#= Version:     1.0.000
#= Revisions:
#= Author       Date        Version Comments
#= ============ =========== ======= ===============================================================
#= bholcombe    01-25-2015  1.0.000 Initial Version
#==================================================================================================

## makeCacheMatrix will accept a matrix variable and return a function where the matrix supplied (x)
## will be embeded in its environment. This returned function will have the set, get, setsolve
## and getsolve functions inside it.  

makeCacheMatrix <- function(x = numeric()) {
  s <- NULL
  
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  
  # Returning a list of the following functions
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## cacheSolve accepts the product of makeCacheMatrix as input variable (x). The end result will be to return the solve (inverse)
## of the matrix from the function as well as to pass the solve back to makeCacheMatrix.

cacheSolve <- function(x, ...) {
  # Retreive the getsolve function from x
  m <- x$getsolve()
  # If m is not null return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Retreieve the data from get (original value supplied to makeCacheMatrix as x)
  data <- x$get()
  # Calculate the solve of data
  m <- solve(data, ...)
  # Pass m (inverse of originally supplied matrix) to the setsolve function (makeCacheMatrix will store this value)
  x$setsolve(m)
  # Return a matrix that is the inverse of 'x'
  m
}

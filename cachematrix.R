## Programming Assignment 2
## Dorothy Sandefur

## makeCacheMatrix & cacheSolve are functions that use inverted special matrix objetcs.
## Tehse functions are done to reduce computations costs.
#
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    z <- NULL #will b used to clear cache
  set <- function(y) {
          x <<- y
          z <<- NULL # clears cash
  }
  get <- function() x #function that finds the matrix's value
  setinverse <- function(inverse) z <<- inverse #sets inverse function
  getinverse <- function() z #defines the inverse function
          
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)



##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
###If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  z <- x$getinverse() #retrieves cache values
  if (!is.null(z)) { #cache null check
          message("Retrieveing cached data")
          return(z)
  }
  data <- x$get()   #retrieve matrix values
  z <- solve(data, ...)   #calculate the inverse matriz
  x$setinverse(z)         #cache results
  z
}

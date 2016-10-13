## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverseVar <- NULL
  set <- function(y) {
    x <<- y
    inverseVar <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inverseVar <<- inverse
  getInverse <- function() inverseVar
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverseVar <- x$getInverse()
  if(!is.null(inverseVar)) {
    message("getting cached data")
    return(inverseVar)
  }
  data <- x$get()
  inverseVar <- solve(data, ...)
  x$setInverse(inverseVar)
  inverseVar
}
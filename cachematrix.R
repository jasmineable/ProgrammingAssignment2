
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
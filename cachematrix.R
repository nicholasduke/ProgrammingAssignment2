## The makeCacheMatrix function creates a matrix that can cache its inverse. 
## This is under the assumption that the input is an invertible square matrix. 

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function solves for the inverse of the matrix that is provided by makeCacheMatrix.
## If the inverse has already been provided then this function will access that from the cache.

cacheSolve <- function(x, ...) {
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

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix is a function that takes a matrix as an argument
# and returns an object with four functions: set(), get(), setinv(), and 
# getinv().

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve is a function that takes a makeCacheMatrix object as input and
# calculates the inverse of the matrix, but if the inverse has already been 
# calculated, it returns the inverse from the cache without recalculating it.

cacheSolve <- function(x, ...) {
  i <- x$getinv
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

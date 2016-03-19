## The pair of functions makeCacheMatrix and cacheSolve 
## allow the user to save time and trouble by caching the
## inverse of a matrix. These functions assume a square, 
## invertible matrix.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function computes the inverse of the special 
## object returned by makeCacheMatrix. It checks to see if the inverse has 
## already been calculated, and that the matrix remains unchanged.  If so, 
## the cacheSolve function retrieves the inverse from the cache. 

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}

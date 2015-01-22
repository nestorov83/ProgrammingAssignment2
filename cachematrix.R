
## makeCahceMatrix and cacheSolve are a pair of functions that invert a matrix and cache it 
## the caching takes place in the environment of makeCahceMatrix, which is a new instance 
## for any given matrix (i.e. defining a new "cached matrix" will create a new envrironment).
## This allows calling the inverted matrix from the cache if it has been calculated already,
## otherwise it will be solved in cacheSolve, which gets the data from makeCachedmatrix.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}



cacheSolve <- function(x, ...) {
  m <- x$getSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}

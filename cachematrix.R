
## makeCahceMatrix and cacheSolve are a pair of functions that invert a matrix and cache it in the environment of makeCahceMatrix
## this allows calling the inverted matrix from the cache if it has been calculated already,  otherwise it will be solved from the cacheSolve function. 



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
        ## Return a matrix that is the inverse of 'x'
}

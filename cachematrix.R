## Function that can creates a special "matrix" object that can cache its inverse.
## Input: Matrix
## Output: set,get, setmean and getmean 
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  getmean <- function() m
  setmean <- function(mean) m <<- mean
  list(get = get, set = set, getmean = getmean, setmean = setmean)
}

## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix
##  If the matrix has not changed, then cacheSolve should retrieve the inverse from the cache.
## Input : Matrix
## Output: Inverse Matrix

cacheSolve <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}

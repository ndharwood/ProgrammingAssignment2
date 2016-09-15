## Functions cache the inverse of a matrix rather than computing it repeatedly to save time.

## This function creates a special matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  cache = NULL
  setMatrix <- function(y) {
    x <<- y
    cache <<- NULL
  }
  getMatrix <- function() x
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  getInverse <- function() cache
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


## This function computes in inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(y, ...) {
  inverse <- y$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- y$getMatrix()
  inverse <- solve(data, ...)
  y$cacheInverse(inverse)
  inverse
}
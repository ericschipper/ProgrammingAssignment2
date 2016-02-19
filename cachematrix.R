## These functions are intended to enable caching and retrieving both a matrix and its inverse
## in a single object created by "makeCacheMatrix".


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(solve) inverse <<- solve
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## check the cache
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    ## if inverse is not NULL, it has already been calculated and cached
    message("getting cached data")
  }
  else {
    ## if inverse is NULL, calculate and cache it
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
  }
  
  ## return the inverse - doesn't matter at this point if it was retrieved or calculated
  inverse
}



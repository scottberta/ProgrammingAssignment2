## The following 2 functions cache the inverse of a matrix

## create a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invrs <<- inverse
  getInverse <- function() invrs
  list( set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}


## This fumction computes the inverse of the "special" matrix 
## returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInverse()
  if(!is.null(invrs)) {
         message("getting cahed data")
         return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setInverse(invrs)
}


## This will calcualte the Matrix inverse of the Matrix, If the inverse is
## available from cached data, it will source it from the cache else comptes
## the inverse

## This function creates the inverse of the matrix and places it in cache.

makeCacheMatrix <- function(x = matrix()) {
  ## Creates a list of functions that
  ## can cache the inverse of a matrix.
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<-inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## If the Matrix inverse is already cached, source the output from cache else 
## compute the inverse. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Computes the inverse of the matrix returned
  ## by makeCacheMatrix(), unless the inverse has
  ## already been calculated, in which case
  ## it retrieves it from the cache.
  m <- x$getInverse()
  if ( ! is.null(m)) {
    print("getting cached data")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}

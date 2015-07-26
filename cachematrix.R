## This is a program to compute the inverse of a matrix, and store in cache.
## If the inverse of same matrix is needed once again, it will be returned from cache.

## This function stores the matrix in cache

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function computes the inverse of matrix, or returns the cache'd result

cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInv(m)
  m
}

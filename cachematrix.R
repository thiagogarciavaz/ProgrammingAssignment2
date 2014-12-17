## Creates a matrix with attributes that allow you store/recover 
## its inverse to/from cache (less costly).
## x parameter contains the original matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # this super-assignment operator updated the value in containing environment
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Returns the inverse from a matrix, either from cache or new calculus

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Getting cached inverse...")
    return(inv)
  }
  # if not cached yet, it calculates the inverse through solve function and calls setinverse to store in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
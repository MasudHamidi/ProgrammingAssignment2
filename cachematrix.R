##
## Inverse matrix calculation with cache for calculated results, so you don't do the same calculation twice.
##

## Make the cache matrix - a list with getter, and setter functions for a matrix, and its inverse
## Parameter x: The matrix to wrap up
## Returns: A special matrix with x in it
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # Getters, and setters for a matrix, and its inverse matrix
  list(
    # Set matrix
    set = function(y) {
      x <<- y
      m <<- NULL
    },
    # Get matrix
    get = function() x,
    # Set inverse
    setinverse = function(inverse) m <<- inverse,
    # Get inverse
    getinverse = function() m
  )
}

## Calculate inverse matrix, if not already calculated. Returns result, and stores it for later usage.
## Parameter x: A special matrix created by function makeCacheMatrix
## Parameter ...: Additional parameters for the solve function
## Returns: The inverse matrix
## Side effect: Store the inverse matrix for subsequent calls.
cacheSolve <- function(x, ...) {
  # Check, if we already have inverse?
  m <- x$getinverse()
  if(!is.null(m)) {
    # Yes: Return pre-calculated inverse
    message("getting cached data")
    return(m)
  }
  # No: Calculate the inverse, and store it
  m <- solve(x$get(), ...)
  x$setinverse(m)
  # Return a matrix that is the inverse of 'x'
  m
}

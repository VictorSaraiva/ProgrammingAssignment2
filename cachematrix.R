#############################################################  
# Description: functions that cache the inverse of a matrix.
#
# version   date        reason                Author
# 1.0       23/12/2015  Initial Version       Victor Saraiva
#############################################################


# MakeCacheMatrix() creates a special "matrix" object
# that can cache its inverse.
# get() returns the cached atrix
# set() caches a new matrix
# setsolve caches the value of an inversed atrix
# getsoleve returns the cached inversed matrix

makeCacheMatrix <- function(x = matrix()) {
  # Set cache for inverse matrix to null
  m <- NULL
  # Set x to the new matrix passed in 
  # and value of inversed matrix to NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Return the matrix 
  get <- function() x
  # Cache the value of solve
  setsolve <- function(solve) m <<- solve
  # Return the value od solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)

}


# cacheSolve computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Is it in the cache?      
  m <- x$getsolve()
  if(!is.null(m)) {
    # Yes it is 
    message("getting cached data")
    return(m)
  }
  # No it's not
  # Get the matrix
  data <- x$get()
  # Calculate the inverse
  m <- solve(data, ...)
  # and finally cache it
  x$setsolve(m)
  m
}

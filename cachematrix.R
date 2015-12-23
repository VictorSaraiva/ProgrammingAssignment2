#############################################################  
# Description:
# MakeCacheMatrix() caches a Matrix and value of solve.
# cacheSolve() returns value of solve by obtaining from the cache
# or calculating the inverse of matrix.
#
# version   date        reason                Author
# 1.0       23/12/2015  Initial Version       Victor Saraiva
#############################################################


# MakeCacheMatrix() caches a matrix and value of solve.
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


# cacheSolve return a matrix that is the inverse of 'x'
# by retrieving from the cache or calculating it

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

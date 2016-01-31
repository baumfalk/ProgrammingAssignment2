# This file contains two functions:
#makeCacheMatrix: This function creates a special "matrix" object that can 
#                 cache its inverse.
#
#cacheSolve: This function computes the inverse of the special "matrix" returned
#            by makeCacheMatrix above. If the inverse has already been calculated
#            (and the matrix has not changed), then the cachesolve should retrieve
#            the inverse from the cache.

# Usage example:
# a <- matrix(c(1,2,3,0,1,4,5,6,0),nrow=3,ncol=3) # an inversable matrix
# b <- makeCacheMatrix(a) # convert the matrix to a cachable matrix
# c <- cacheSolve(b) # calculate the inverse for the first time
# d <- cacheSolve(b) # calculate the inverse for the second time, "getting cached data" is now printed in the console.

# The function makeCacheMatrix, given a matrix object, returns a special 
# matrix object that can cache its inverse. 
makeCacheMatrix <- function(x = matrix()) {
  # the inverse is initialized as null
  inverse <- NULL
  # the set function for the matrix
  # if a new matrix is added, delete the current inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  #the get function returns the original matrix
  get <- function() x
  # setinverse sets the inverse of the matrix
  setinverse <- function(inverse) inverse <<- inverse
  
  #getinverse returns the inverse of the matrix
  getinverse <- function() inverse
  # returns the function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The function cacheSolve returns the inverse of a matrix created using
# the makeCacheMatrix. If the inverse of the (unchanged) matrix has
# been computed before, it returns the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  # Try to retrieve the inverse, and return it if it already exists
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  # The inverse doesn't yet exist, retrieve the original matrix...
  data <- x$get()
  # ... compute the inverse and cache it
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  # ... and return the inverse!
  inverse
}
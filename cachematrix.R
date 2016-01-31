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
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# The function cacheSolve returns the inverse of a matrix created using
# the makeCacheMatrix. If the inverse of the (unchanged) matrix has
# been computed before, it returns the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
## There are two functions in this document makeCacheMatrix and cacheSolve. 
## makeCacheMatrix is a constructer which can be used to create an object which exposes 
## methods to stroe/retrieve the value of a matrix. It also provides methods to store/retrieve
## inverse of the matrix.
## The other function cacheSolve is a function to actually calculate the inverse of matrix
## if the Cahce object already doesn't have the inverse value calculated. 


## This function create an object from a matrix and returns a list of methods available.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes the cache object as input and sets the inverse if the inverse value is not already
## available in the object. It returns the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

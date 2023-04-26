## The aim of the makeCacheMatrix function is to create a matrix that can cache its inverse.
## Then, the cacheSolve matrix computes the inverse of the matrix returned by the makeCacheMatrix function.

## The makeCacheMatrix function consists of four functions:
## set, which sets the value of the matrix and also sets the cached inverse to NULL (i.e., the null object)
## get, which gets the value of the matrix
## setinv, which sets the cached inverse of the matrix, and
## getinv, which gets the cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  #We set the value of the matrix and the cached inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##We get the value of the matrix
  get <- function() x
  
  ##We set the cached inverse of the matrix
  setinv <- function(inverse) inv <<- inverse
  
  ##We get the cached inverse of the matrix
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the inverse of the matrix object created by the makeCacheMatrix function.
## If the cached inverse is not NULL, it retrieves the cached inverse without recomputing it.
## If the cached inverse is NULL, it computes the inverse using the solve function, sets the cached inverse using the setinv function, and returns the inverse.

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
        
  if(!is.null(inv)) {
          message("getting cached inverse")
          return(inv)
  }
  
  M <- x$get()
  inv <- solve(M, ...)
  x$setinv(inv)
  print(inv)
  
}
## This file contains two functions makeCacheMatrix and cacheSolve
## The functions are designed to avoid the costly computation of matrix inversion by caching the
## inverse of the matrix if the inverse has been previously computed for the matrix.

## Write a short comment describing this function
## This function creates a special matrix object that caches the input matrix and its inverse
## The inverse of the matrix is computed using the solve function
## KEY ASSUMPTION: The matrix passed to the function is invertible.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

## Returns the inverse of a matrix passed to it. The function first checks if the inverse of the passed matrix
## has already been computed and is available in the cache. If so, it will return the cached inverse.
## Otherwise, it will compute the inverse using the solve function and cache the inverse.
## KEY ASSUMPTION: The matrix passed to the function is invertible.
cacheSolve <- function(x=matrix(), ...) {
        if (is.matrix(x)) {
          stop("Please invoke makeCacheMatrix first before invoking cacheSolve...exiting...")
        }
          
        m <- x$getinverse()
        if(!is.null(m)) {
          message ("Returning cached inverse")
          return(m)
        }        
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m  
}

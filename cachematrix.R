## This two functions are the solution to the Programming Assigment 2 
## from Coursera's R Programming course:
## 1. Function makeCacheMatrix creates a "matrix" object that
## can cache its inverse.
## 2. Function cacheSolve computes the inverse of "matrix" returned 
## by makeCacheMatrix.


## Creates a "matrix" object that can cache its inverse
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

## Computes the inverse of "matrix" returned by makeCacheMatrix but if
## the inverse has already been calculated (and the matrix has not changed), 
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
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
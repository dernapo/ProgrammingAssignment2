## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. We will cache the inverse of a matrix by using the following functions.

## The following function creates a special "matrix" object that can cache its inverse.
## The special "vector", which is really a list containing a function to
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <-function (y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(mt) m <<- mt
        getinverse <- function() m
        list(set = set, get=get, 
               setinverse = setinverse,
               getinverse = getinverse)
}


## This second function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. 
## And returns a matrix that is the inverse of 'x'
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        mt <- x$getinverse()
        if(!is.null(mt)) {
                message("getting cached data")
                return(mt)
        }
        data <- x$get()
        mt <- solve(data, ...)
        x$setinverse(mt)
        mt
}

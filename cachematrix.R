## Matrix inversion is usually a costly computation and there
## may be some benefit to caching the inverse of a matrix rather
## than compute it repeatedly. These functions compute and cache
## the matrix inverse.
##
## These functions assume that the matrix supplied is invertible.


## makeCacheMatrix:
## This function creates a special "matrix" with functions to
## get, set, getsolve, and setsolve.
##
## 'x' is a matrix that is wrapped with additional functionality.
##
## Return a "special" matrix that is able to cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve:
## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
##
## 'x' is a special "matrix" created by calling makeCacheMatrix
##
## Return the inverse of the special "matrix"
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

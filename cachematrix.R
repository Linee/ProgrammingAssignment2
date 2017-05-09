## Programming Assignment 2 Coursera - R: Caching the Inverse of a Matrix

# This file provides two functions that cache the inverse of a matrix in order to 
# speed up the computation process.

## The first function (makeCacheMatrix) creates a special "matrix" object that can cache 
# its inverse, using the solve function of R (solve(X)). For example, if X is a square 
# invertible matrix, then solve(X) returns its inverse.


makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y){
        x <<-y
        inverse<<-NULL
    }
    get <-function() x
    setInverse <-function(inverse) inverse <<- inverse
    getInverse <- function() inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The second function (cacheSolve) computes the inverse of the special "matrix" returned above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache. Otherwise it computes the inverse and sets the value.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <-  x$get()
    inverse <- solve(data, ...)   ## Return a matrix that is the inverse of 'x'
    x$setInverse(inverse)
    inverse
}


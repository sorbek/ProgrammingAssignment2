## From the program assignment:
##     "Matrix inversion is usually a costly computation and there may be
##      some benefit to caching the inverse of a matrix rather than compute
##      it repeatedly."

## The functions makeCacheMatrix and cacheSolve enable caching the inverse
## of a matrix.

## Create a special matrix object that can cache its inverse.
##   Adapted from the makeVector example presented in the assignment.
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Computes the inverse of the special matrix returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
##   Adapted from the cachemean example presented in the assignment.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if ( !is.null(i) ) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}


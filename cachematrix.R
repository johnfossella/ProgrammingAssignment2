
## For an invertable matrix this script creates
## a list of functions sets and gets the matrix
## and also sets and gets the inverse of the matrix. 
## These are used by the cacheSolve script.

makeCacheMatrix <- function(x = matrix()) {
    inv = NULL
    set = function(y) {
        x <<- y
        inv <<- NULL
    }
    get = function() x
    setinverse = function(inverse) inv <<- inverse 
    getinverse = function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## This script gets the output of makeCacheMatrix()
## and uses it to calculate the inverse.
## Then it sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
    inv = x$getinvers()
    if (!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    matrixvalues = x$get()
    inv = solve(matrixvalues, ...)
    x$setinvers(inv)
    return(inv)
}

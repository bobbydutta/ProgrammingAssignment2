## These functions are used to optimize the computation of matrix inversion
## by caching the inverse of a matrix and retrieving it from cache rather than
## computing it again and again. 



## This function creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function( x = matrix() ) {
n <- NULL
set <- function( matrix ) {
            		x <<- matrix
            		n <<- NULL
    }
    get <- function() x
        setInverse <- function(inverse) n <<- inverse
        getInverse <- function()         n
        list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function calculates the inverse of the special 'matrix' returned by 
##the makeCacheMatrix function above. If the inverse has already been calculated
## and the matrix has not changed, then cacheSolve should retrieve the inverse
## from the cache.


cacheSolve <- function(x, ...) {
        n <- x$getInverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setInverse(n)
        n
}

## This is analogue to the example:
## It provides a data structure to store a matrix and its inverse.
## A scond function allows to cache the inverse when calculated.

## Defines a data structure of a matrix with the functions of
## get, set, getinv and setinv.

makeCacheMatrix <- function(x = matrix()) {
    ## Takes a matrix, will be square invertible.
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setinv <- function(m) minv <<- m
        getinv <- function() minv
        
    ## Returns a list with functions to interact with stored matrix.
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


cacheSolve <- function(x, ...) {
    ## Input is a makecacheMatrix$get.
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        
        ## Returns a matrix that is the inverse of 'x'
        m
}
}

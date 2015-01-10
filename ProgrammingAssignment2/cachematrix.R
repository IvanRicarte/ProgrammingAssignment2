## These functions were developed as part of Assignment 2 for 
## Coursera's R Programming Course
## January, 2015

## makeCacheMatrix creates a data structure (object) to represent a matrix
## and its (cached) inverse, along with the functions (methods) to handle this data

makeCacheMatrix <- function(x = matrix()) {
        # initial state: matrix (x) and inverse (ix) NULL
        ix <- NULL
        # set initial matrix value (its inverse is still NULL)
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        # get the matrix value
        get <- function() x
        # update the cache for the inverse of the matrix
        setinv <- function(invx) ix <<- invx
        # get the cached version of the inverse of the matrix
        getinv <- function() ix
        # return the structure with the four "methods"
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns a matrix that is the inverse of 'x'.
## It uses a makeCacheMatrix object to save the computation 
## effort if a cached version already exists in that object.

cacheSolve <- function(x, ...) {
        # Is there a cached version of the inverse of x?
        invx <- x$getinv()
        if(!is.null(invx)) {
                # Yes, return it.
                return(invx)
        }
        # No, let us compute it
        mat <- x$get()
        invx <- solve(mat, ...)
        # Let us save (cache) it for future use
        x$setinv(invx)
        # And, now, return it.
        invx
}

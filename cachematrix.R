## Functions that enable to cache the result of inverting a matrix
## This way the inverse is calculated only once per each such matrix

## makeCacheMatrix extends the capabilities of a matrix so that it
## stores the inverse of the matrix (for later access). The inverse 
## can be accesssed through setInverse and getInverse functions

makeCacheMatrix <- function(x = matrix()) {
    matrInv <- NULL
    
    ## sets up the 'special matrix' object, matrix inverse is set to NULL
    set <- function(matr) {
        ## the variables we assign values to belong to the
        ## 'outer' environment thus we are using <<- operator
        x <<- matr
        matrInv <<- NULL
    }
    
    ## returns the matrix data
    get <- function() x
    
    ## caches the matrix inverse
    ## the matrInv variable belongs to 'outer' environment so we use <<-
    setInverse <- function(inv) matrInv <<- inv
    
    ## returns the cached matrix inverse
    getInverse <- function() matrInv
    
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## cacheSolve calculates the inverse of a matrix, but only on first call.
## In all successive calls the cached value (stored during the first call
## of the function) is returned.

cacheSolve <- function(x, ...) {
    ## retrieve the cached value. If it exists (is not NULL) then just
    ## return the value, otherwise calculate the inverse and cache it
    matrInv <- x$getInverse()
    if(!is.null(matrInv)) {
        ## returning the cached value
        return(matrInv)
    }
    matrData = x$get()
    ## calculating ...
    matrInv <- solve(matrData, ...)
    ## and cacheing the calculated value
    x$setInverse(matrInv)
}

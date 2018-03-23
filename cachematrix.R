## A pair of functions that cache the inverse of a matrix.

## Creates a 'matrix' object. a 'matrix' object contains the functions:
##
## get(), which gets the matrix
## set(y), which sets a new value for the matrix and sets the cached inverse to NULL
## getinverse(), which gets the inverse matrix
## setinverse(inverse), which sets the value for the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function checks if there is a not NULL cached inverse for the 'matrix'
## object. If there is, it returns this inverse matrix.
## If not, it calculates it, caches it and returns it.

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

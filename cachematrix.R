## The functions below serve to cache the inverse of a matrix in order to 
## avoid repeatedly computing it.

## This function creates a special kind of matrix that will serve as an
## input to the next function that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Finds the inverse of a matrix by first looking if it is cached, otherwise
## it computes it and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- ginv(data, ...)
        x$setinv(inv)
        inv
}

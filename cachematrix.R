## This file contains the functions required to cache the inverse of 
## a supplied matrix.

## makeCacheMatrix creates the functions required to cache and retrieve the
## value of the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve first checks the cache for a precomputed inverse
## otherwise it computes it directly

cacheSolve <- function(x, ...) {
        ## Check for a precomputed inverse.  If m is null not then the inverse was cached
		## notify that we are using a cached value and return the inverse
		m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		
		## If we get here then the inverse was not cached, compute it and cache the result
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
		
		## return the computed inverse
        m
}

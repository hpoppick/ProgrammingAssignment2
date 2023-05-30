## Set clears the cached data and resets stored matrix to new matrix
## Get retrieves the stored matrix
## Setsolve uses solve to store the inverse of the matrix to the cache
## Getsolve retrieves cached values of the matrix inversion
## The list creates labels for set, get, setsolve and getsolve to allow
## for referencing using $

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}


## The following checks whether or not the cache contains
## relevant data and if so, retrieves it. Otherwise it
## solves the matrix inversion and fills the cache with it.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m

}

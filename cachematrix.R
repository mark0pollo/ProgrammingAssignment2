## This pair of functions will calculate the inverse of a matrix
## only if the inverse has not already been solved. If it has been solved
## it will retrieve the inverse from cached memory

## makeCacheMatrix creates a list containing the following functions
## set - Allows you to set the value of a matrix
## get - retrieves teh value of a matrix
## setInverse - sets the inverse of the matrix
## getInversee - retrieves the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Given a matrix x it will check to see if the matrix inverse is already
## stored in memory. If so it will return that value. Otherwise it will
## solve the inverse and store the solution in memory.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

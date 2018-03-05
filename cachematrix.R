## These functions implement a caching system to reduce the time required to
## calculate the inverse of a matrix. As this is a a resource-intensive
## operation, the inverse is cached after the first time it is calculated,
## allowing subsequent calls to get the inverse to complete much more quickly.
## These functions serve as my submission to Programming Assignment 2 in the
## Johns Hopkins University Data Science Specialization, R Programming class.
## Andy Tinkham, andy@tinkham.org, March 4, 2018.

## makeCacheMatrix defines a custom "matrix" object capable of caching its
## inverse. It defines a list containing functions to set a matrix, to get that
## matrix back, to store the value of the matrix's inverse, and to retrieve
## that cached inverse value.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve first checks to see if the inverse of the matrix has already been
## computed. If it has, it returns the cached version. Otherwise, it computes
## the matrix's inverse and then stores that inverse in cache to retrieve on
## future calls.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        message("calculating inverse")
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

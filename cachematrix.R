
## Suhkyung Kim
## R programming Assignment 2 -- Create a function for calculating a square matrix
## and return its invert value 

## This function creates a special "matrix" object that can cache its inverse
## Warning: To run this function without error, matrix input should be invertible

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) x <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mt <- x$getinverse()
        if(!is.null(mt)) {
                message("getting inversed matrix")
                return(mt)
        }
        data <- x$get()
        mt <- solve(data, ...)
        x$setinverse(mt)
        mt
}

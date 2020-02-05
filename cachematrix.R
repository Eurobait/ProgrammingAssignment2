## A group of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(inverse) {
        m <<- inverse
    }
    getInverse <- function() {
        m
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the inverse in the cache via the setInverse function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting Cached Inverse Matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}


## Assignment Demo Code
source("cachematrix.R")
M <- matrix(c(1,2,3,4),2,2)
M
M1 <- makeCacheMatrix(M)
## printing the inverse of matrix
cacheSolve(M1)
## Run again to find the inverse
cacheSolve(M1) 
## It prints the message:Getting Cached Inverse Matrix



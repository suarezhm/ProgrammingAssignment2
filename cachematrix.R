## The functions makeCacheMatrix and cacheSolve take advantage of the Lexical
## Scoping rules of the R language to cache potentially time-consuming
## computations by storing inside an R object a previously calculated inverse of
## a matrix, since matrix inversion is usually a costly computation.


## The makeCacheMatrix function creates a special "matrix" object that can 
## cache its inverse. It is a list containing a function to:
##   1. set the value of the matrix 
##   2. get the value of the matrix
##   3. calculate the value of the inverse matrix
##   4. get the value of the inverse matrix

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


## The cacheSolve function calculates the inverse matrix of the special "vector" 
## created with the above function. It first checks to see if the inverse matrix
## has already been calculated. If so, it returns the inverse matrix from the
## cache and skips the computation. Otherwise, it calculates the inverse matrix of
## the data and sets the value of the inverse matrix in the cache via the 
## setinverse function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting inverse matrix data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
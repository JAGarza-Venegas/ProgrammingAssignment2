## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function.

## This function creates a special “matrix”, which is really a list containing a function to
## set the value of the matrix, get the value of the matrix, set the value of the inverse of
## the matrix, and get the value of the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) m <<- solve
    getSolve <- function() m
    list(set = set, get = get, 
         setSolve = setSolve, 
         getSolve = getSolve)
}

## Write a short comment describing this function.

## The following function calculates the inverse of the special “matrix” created with the 
## above function. However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the matrix and sets the value of the inverse in the cache via the 
## setSolve function.

cacheSolve <- function(x, ...) {
    m <- x$getSolve()
    if(is.null(m)){
        message("getting cached matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setSolve(m)
    m
    ## Return a matrix that is the inverse of 'x'
}

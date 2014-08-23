## Put comments here that give an overall description of what your
## functions do:

## The assignment is to write a pair of functions that
## cache the inverse of a matrix.

## 'makeCacheMatrix': This is the first function. It creates a matrix object
## that can cache its inverse.

## 'cacheSolve': This ist the second function. It computes the inverse of the
## matrix returned by `makeCacheMatrix`. If the inverse has
## already been calculated (and the matrix has not changed), then
## 'cacheSolve' retrieve the inverse from the cache.



## Write a short comment describing this function:

## The first function, 'makeCacheMatrix' creates a matrix, which is
## a list containing a function to

##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse



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


## Write a short comment describing this function:

## The function 'cacheSolve' calculates the inverse of the matrix
## created with the function 'makeCacheMatrix'. However, it first checks to see if the
## inverse has already been calculated. If so, it get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and sets the value of the inverse in the cache via the 'setsolve'
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

## This is an example matrix to test the functions:
B = matrix( 
    c(2,1,5,3), 
    nrow=2, 
    ncol=2) 
B

A <- makeCacheMatrix(B)
cacheSolve(A)


##The result is:
##[,1] [,2]
##[1,]    3   -5
##[2,]   -1    2
    

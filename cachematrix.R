## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache 
## its inverse.
## The function, makeCacheMatrix creates a special "vector", 
## which is really a matrix containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}


## Write a short comment describing this function
## The cacheSolve function calculates the inverse matrix 
## of the special "matrix" created with the makeCacheMatrix function. 
## It first checks to see if the inverse matrix has already 
## been calculated. 
## If so, it gets the inverse matrix from the cache and skips 
## the computation. 
## Otherwise, it calculates the inverse matrix of the data 
## and sets the value of the inverse matrix in the cache 
## via the setmatrix function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}

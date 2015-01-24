##R-prog010 course Assignment 2
##24 Jan 2015

## This function stores a matrix and its inverse
## This function supports 4 functions
## 1. Set the value of the matrix
## 2. Retrieve the value of the matrix
## 3. Set the inverse of the matrix
## 4. Retrieve the inverse of the matrix

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

## This function returns the inverse of the matrix
## The function firsts check if the inverse is pre-computed. If that is the case, it returns the cached value of the inverse
## If the inverse is not computed, it computes the inverse, cahces the computed inverse and returns the same

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

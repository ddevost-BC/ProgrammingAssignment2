## These functions eliminate the need to repeatedly compute the inverse a matrix
## by caching the matrix and its inverse once computed.


## makeCacheMatric is a function that:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve is a function that:
## 1. checks to see if the inverse of the matrix has already been computed
## 2. if the inverse of the matrix has already been computed, it returns the inverse of the matrix
## 3. if the inverse of the matrix has not yet been computed, it gets the matrix, solves for the inverse, and caches it

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached matrix.")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix)
    x$setinverse(inv)
    inv
}

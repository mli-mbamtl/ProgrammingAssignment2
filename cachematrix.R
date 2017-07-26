## Below are two functions that are used to create a special
## object that stores a matrix and caches its inverse.


## The first function creates a special "matrix", 
## which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function () m
    list(set = set, get = get, 
         setmatrix = setmatrix, 
         getmatrix = getmatrix)
}

## This function calculates the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the
## matrix has not changed), then the cacheSolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

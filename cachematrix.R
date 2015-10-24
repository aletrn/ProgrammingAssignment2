## Put comments here that give an overall description of what your
## functions do

## 'makeCacheMatrix' creates a special matrix object that can cache its inverse
# eg.: M <- cbind(c(1, -2), c(-2, 1))
#      invM <- makeCacheMatrix(M)
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## 'cacheSolve' computes the inverse of the special "matrix" returned by 
# 'makeCacheMatrix'. If the inverse has already been calculated, and the matrix
# has not changed, then 'cacheSolve' should retrieve the inverse from the cache
# eg.: calling 1st time: cacheSolve(invM) # calculate inverse...
#      calling 2nd time: cacheSolve(invM) # matrix unchanged? get cached data
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
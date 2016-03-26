##Overall Description: The following functions cache the inverse of a matrix.

## 1.makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
##   above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
##   should retrieve the inverse from the cache.

## The first function, makeCacheMatrix creates a special "matrix",
## which is really a list containing a function to:
##1.set the value of the matrix ; 2.get the value of the matrix
##3.set the inverse matrix of the matrix; 4.get the inverse matrix of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


##The cacheSolve function below calculates the inverse matrix of the special "matrix"
##using the makeCacheMatrix function given above.It first checks if the inverse has been 
##computed or not. If yes, it gets the inverse from the cache and skips the computation. 
##Or else, it calculates the inverse of the data and sets the value of the inverse in the 
##cache via the setinverse function.  

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

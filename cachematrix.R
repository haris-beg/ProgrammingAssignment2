## These functions enable us to use cached values of the inverse of a matrix.
## This prevents us from unnecessarily performing costly inversions.

## This function 
## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    invMtrx <- NULL
    set <- function(y) {
        x <<- y
        invMtrx <<- NULL
    }
    get <- function() x
    setInv <- function(inverseMatrix) invMtrx <<- inverseMatrix
    getInv <- function() invMtrx
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function 
## computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If a cached value is available, then that is returned.
## Otherwise a new inverse is calculated.
cacheSolve <- function(x, ...) {
    ## First check the cache for the inverse of 'x'
    invMtrx <- x$getInv()
    if(!is.null(invMtrx)) {
        message("getting cached data")
        return(invMtrx)
    }
    
    ## If cached value not available, then compute the inverse of 'x'
    data <- x$get()
    invMtrx <- solve(data, ...)
    x$setInv(invMtrx)
    
    ## Return a matrix that is the inverse of 'x'
    invMtrx
}

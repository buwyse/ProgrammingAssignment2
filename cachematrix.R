## https://github.com/buwyse/ProgrammingAssignment2.git
## SHA-1 hash identifier: 
## Cache create and invert matrices to reduce processor usage. 
## Assumption is that supplied matrix is always invertible.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ##initialize the inverse matrix variable as null
    xinv <- NULL
    ##create a vectorized object to set, get, set inverse, and get inverse matrices
    set <- function (y) {
        y <<- x
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) xinv <<- solve
    getinv <- function() xinv
    list(set = set,get = get,setinv = setinv,getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    z <- x$get()
    if(!is.null(xinv)){
        message("retrieving the cached matrix")
        return(xinv)   
    }
    xinv <- solve(z)
    x$setinv(xinv)    
    xinv
}

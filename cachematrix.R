## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    getInv <- function() inv
    setInv <- function(inv_x) inv <<- inv_x
    list(get = getInv, set = setInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    inv_x <- solve(x)
    inv$setInv(inv_x)
    inv
}

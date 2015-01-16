## makeCacheMatrix and cacheSolve create and solve/inverse a matrix.
## Because large matrices require a lot of computing ressources it may be
## helpful to cache the results of an inverted matrix
## cacheSolve calls makeCacheMatrix in order to cache or retrieve the 
## matrix as well as the inverted matrix.

## Usage examples:
##
## > mat=rbind(c(1, 2), c(11, 12))
## > class(mat)
## [1] "matrix"
## > cachemat <- makeCacheMatrix(mat)
## > cachemat$get()
     ## [,1] [,2]
## [1,]    1    2
## [2,]   11   12
## > cachemat$getinv()
## NULL
## > cacheSolve(cachemat)
## Calculating matrix inverse
     ## [,1] [,2]
## [1,] -1.2  0.2
## [2,]  1.1 -0.1
## > cacheSolve(cachemat)
## Retrievibg cached matrix inverse
     ## [,1] [,2]
## [1,] -1.2  0.2
## [2,]  1.1 -0.1

## makeCacheMatrix.set: cache a new matrix. Previously cached data will be
##                      overwritten
## makeCacheMatrix.get: return the cached matrix
## makeCacheMatrix.setinv: cache the inverted matrix
## makeCacheMatrix.getinv: return the cache inverted matrix
## x: a matrix. It must be an invertible matrix. 
##              Otherwise cacheSolve will generate errors
makeCacheMatrix <- function(x = matrix()) {
    inv_mat <- NULL
    set <- function(mat) {
        x <<- mat
        inv_mat <<- NULL
    }
    get <- function() x
    setinv <- function(inv) inv_mat <<- inv
    getinv <- function() inv_mat 
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve: solve/invert a matrix. x must be an invertible matrix
## x: list of functions (it  must be a result of the makeCacheMatrix function)
cacheSolve <- function(x, ...) {
    inv_mat <- x$getinv()
    if (!is.null(inv_mat)) {
        message("Retrievibg cached matrix inverse")
        return (inv_mat)
    }
    mat <- x$get()
    inv_mat <- solve(mat)
    message ("Calculating matrix inverse")
    x$setinv(inv_mat)
    inv_mat
}

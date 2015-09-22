## These functions together implement an object
## which is a "special" matrix that can cache its
## own inverse. This is done in order to save
## repeated calls to create its inverse, since the
## inverse calculation can be expensive.


## this function takes an input matrix x, and
## returns a "special" matrix which caches its
## inverse after computing it once. The get/set
## functions get and set the matrix x. Each time
## the value of x changes, its stored inverse minv
## is set to NULL. setinv/getinv are used for
## getting and setting the inverse.

makeCacheMatrix <- function(x = matrix()) {

    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(input_inv)
        minv <<- input_inv
    getinv <- function() minv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}#makeCacheMatrix


## this function takes as its input an
## object x output from the makeCacheMatrix
## function. x is tested to see whether the
## inverse has been calculated and stored in
## memory. If so, a message is presented
## informing of this fact, and the stored
## inverse is returned. If not, the inverse
## is computed, stored, and returned.

cacheSolve <- function(x, ...) {
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached inverse")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinv(minv)
    minv
}#cacheSolve


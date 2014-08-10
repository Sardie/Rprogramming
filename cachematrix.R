## creates one function that gets or sets the inverse of a matrix from another 
## function that solves the invertible function passed

## the function that is a list of 'get' and 'set' functions for the next function
## to utilise

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(invMatrix) m <<- invMatrix
    getInv <- function() m
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## the function that solves the inverse of the matrix passed in

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
       message("getting cached inversed matrix")
       return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}

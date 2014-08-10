## Creates two functions. The first function features a list of 'get' and 'set'
## functions and allows the creation of a special matrix object that has these 
## list of functions as its properties/attributes.

## The second function solves for the inverse of the special matrix created in the
## first function and stores it back in cache; this function assumes that all 
## matric objects passed into it are invertible matrices.


## This function creates a special matrix object that gives special functions 
## to it. 
## 1) 'get' allows the user to get the values of the originally stored matrix object
## 2) 'set' allows the user to set a new value or replace that of the original  
##    matrix object
## 3) 'getInv' allows the user to get the inverse values of the stored matrix if  
##    the inverse has already be stored in cache.
## 4) 'setInv' allows the user to set the inverse values of the stored matrix after 
##    the inverse values have been solved by the cacheSolve function. 

## Note:'setInv' should not be called even though it is readily available for 
## to call as it will not return the actual inverse of the special matrix object 
## previously created if it was not called using the cacheSolve function.

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


## This function solves for the inverse of the special matrix object stored in cache
## It assumes that the matrix passed as argument is invertible
## This function uses functions from the makeCacheMatrix function to display the 
## inverse matrix if it is already calculated and cached. Else, it will proceed to
## get the special matrix stored and cache the inverse for retrieval later.

cacheSolve <- function(x = matrix(), ...) {
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

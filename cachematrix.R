## Creates a special matrix and calculates the inverse
## Sets and gets inverse value to avoid recalculation

## Function: makeCacheMatrix
## creates a special matrix to be able to set and get matrix value,
## and to set (or save) and get inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function: cacheSolve
## Calculates the inverse of the special matrix created with makeCacheMatrix function. 
## It checks first if inverse value has already been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
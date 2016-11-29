#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

## Write a cache function
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inverse <<- solve
    getsolve <- function() inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## get an inverse matrix from cache if there's one, or make a new one
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    x.inverse <- x$getsolve()
    if(!is.null(x.inverse)) {
        message("getting cached data")
        return(x.inverse)
    }
    data <- x$get()
    x.inverse <- solve(data, ...)
    x$setsolve(x.inverse)
    x.inverse
    
}

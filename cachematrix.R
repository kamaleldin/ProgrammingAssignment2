## Using the solve function to invert a Matrix 
## Caching matrix inversion for a matrix that has already been inverted with the
## help of R scoping

## cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## invert a matrix, if it has the same values retrieve the cached data instead

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- Solve(data)
    x$setsolve(m)
    m
        ## Return a matrix that is the inverse of 'x'
}

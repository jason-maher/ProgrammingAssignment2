## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(data) {
        x <<- data
        inverse <<- NULL
    }
    get <- function() { x }
    setInverse <- function(value) { inverse <<- value }
    getInverse <- function() { inverse }

    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    if (is.null(x$getInverse())) {
        x$setInverse(solve(x$get(), ...))
    }
    x$getInverse()
}

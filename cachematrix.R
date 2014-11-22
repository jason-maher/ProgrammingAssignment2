#' This file loads two functions that assist in calculating the inverse of large
#' matricies. Both functions work on or create a 'cacheMatrix' object with the
#' following properties:
#'
#'  - `set(data)` : sets the matrix data contained in the cacheMatrix, calling this
#'                  method will invalidate any cached inverse.
#'  - `get()`     : returns the data contained in the cacheMatrix. will never return
#'                  null unless null has been explicity set or provided to the
#'                  `makeCacheMatrix(x)` factory method.
#'  - `setInverse(inverse)` : sets the inverse matrix calculated elsewhere and causes
#'                            it to be cached in the instance. See notes below.
#'  - `getInverse()         : returns a cached inverse from the object if one exists. if
#'                            no inverse has been calculated/set this may return null.
#'
#' It is not recommended to call `setInverse()` directly but rather to use `cacheSolve()`
#' instead. See `makeCacheMatrix` and `cacheSolve` for more detail on their operation.

#' Creates a new cacheMatrix object with the properties described above. Initial data
#' can be specified for the new matrix or a default empty one will be created.
#'
#' @param x the data for the created cacheMatrix to contain. default = `matrix()`.
#' @return a new cacheMatrix containing the specified data but without a cached
#'         inverse.
makeCacheMatrix <- function(x = matrix()) {
    # always initialize inverse to NULL
    inverse <- NULL

    # setting new data must invalidate the cache
    set <- function(data) {
        x <<- data
        inverse <<- NULL
    }

    # all other functions are basic setters/getters
    get <- function() { x }
    setInverse <- function(value) { inverse <<- value }
    getInverse <- function() { inverse }

    # construct the 'cacheMatrix' object as a list of the functions above
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

#' Returns the inverse of a cacheMatrix. If the cacheMatrix has a cached inverse already
#' then this function will simply return it. Otherwise the inverse is calculated using
#' `solve` and the result is cached on the cacheMatrix.
#'
#' @param x the cacheMatrix to calculate and inverse for
#' @param ... additional arguments to pass to `solve` if a new inverse is being calculated
#' @return the inverse of the cacheMatrix
#' @references solve
cacheSolve <- function(x, ...) {
    # if no cached inverse exists calculate it and set on the cacheMatrix
    if (is.null(x$getInverse())) {
        inverse <- solve(x$get(), ...)
        x$setInverse(inverse)
    }
    # by this point we're guaranteed a cached value
    x$getInverse()
}

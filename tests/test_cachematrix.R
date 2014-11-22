
#' Creates a square matrix filled with random (normally distributed) data
#' for use in the tests.
#'
#' @param size the number of colums/rows in the created matrix, default = 5.
#' @return a square matrix of the given size with random values.
create.sampleMatrix <- function(size = 5) {
    matrix(rnorm(size*size), ncol = size)
}

#' check the function is doing something
test.makeCacheMatrix.returnsSomething <- function() {
    checkEquals(FALSE, is.null(makeCacheMatrix()))
}

#' check the data contained in the function matches the input args
test.makeCacheMatrix.containsInputData <- function() {
    input <- matrix()
    cm <- makeCacheMatrix(input)
    checkIdentical(input, cm$get())
}

#' check that `setInverse()` does cache the value when called directly
test.cacheMatrix.canSetInverse <- function() {
    fake <- create.sampleMatrix()
    cm <- makeCacheMatrix()
    cm$setInverse(fake)
    checkEquals(fake, cm$getInverse())
}

#' check that setting new data to the matrix wipes the cache so that we don't get
#' old results.
test.cacheMatrix.newDataInvalidatesCache <- function() {
    fake <- create.sampleMatrix()
    cm <- makeCacheMatrix()
    cm$setInverse(fake)
    checkEquals(fake, cm$getInverse())
    cm$set(create.sampleMatrix())
    checkTrue(is.null(cm$getInverse()))
}

#' check that `cacheSolve()` actually does solve correctly
test.cacheSolve.calculatesInverse <- function() {
    sample <- create.sampleMatrix()
    sample.inverse = solve(sample)
    cm <- makeCacheMatrix(sample)
    checkEquals(sample.inverse, cacheSolve(cm))
}

#' check that `cacheSolve()` actually caches the result on the matrix instance
test.cacheSolve.cachesInverse <- function() {
    sample <- create.sampleMatrix()
    sample.inverse = solve(sample)
    cm <- makeCacheMatrix(sample)
    checkEquals(sample.inverse, cacheSolve(cm))
    checkEquals(sample.inverse, cm$getInverse())
    # TODO: It would be nice to somehow verify that further invocations of cacheSolve
    #       don't recalculate the inverse, not just verify that the result was saved
}

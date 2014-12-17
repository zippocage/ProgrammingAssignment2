## This file contains two functions makeCacheMatrix and cacheSolve
## The functions calculates the inverse of a matrix but with the
## added benefit that the result is cached and will not be recalculated
## if it has already been solved before.

## makeCacheMatrix
## This function caches the matrix and the result of the inversematrix operation.
## The function has a set of internal functions that help cache the matrix
## and the inverse matrix.
## If the matrix changes the cached result will also be deleted.

makeCacheMatrix <- function(x = matrix()) {
  # first reset the inv matrix cache
  invmtrx <- NULL
  ## set - caches a new matrix and resets the inv matrix cache
  set <- function(y) {
    x <<- y
    invmtrx <<- NULL
  }
  ## get - returns the matrix itself
  get <- function() x
  ## setinvmatrix - store a solution to the inverse matrix
  setinvmtrx <- function(inv) invmtrx <<- inv
  ## getinvmatrix - retrieves the cached inv matrix
  getinvmtrx <- function() invmtrx
  list(set = set, get = get,
       setinvmtrx = setinvmtrx,
       getinvmtrx = getinvmtrx)
}

## cacheSolve
## Returns a matrix that is the inverse of 'x' and caches the result.
## If a cahed solution already exists this will be retrieved and returned.

cacheSolve <- function(x, ...) {
  # Check if a cached solution already exists
  invmtrx <- x$getinvmtrx()
  if (!is.null(invmtrx)) {
    message("getting cached solution")
    return(invmtrx)
  }
  # A cached solution does not exist, therefore calculate the inverse matrix
  # and cached it.
  data <- x$get()
  invmtrx <- solve(data, ...)
  x$setinvmtrx(invmtrx)
  invmtrx
}

## These two functions work together to reduce computational expense of matrix
## inversion by creating a cached matrix object in the parent environment. Inversion 
## can be performed and then called from the cache at a later time
## instead of recomputing every time the value is needed.

## Caches the input matrix in an environment other than the current one and 
## creates/returns a list of functions that can be referenced and performed on 
## the cached matrix

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) mat <<- solve
  getInverse <- function() mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Retrieves the cached matrix as input, checks to see if inversion has already been
## calculated (and thus stored), performs (if necessary) matrix inversion, sets the 
## inverted matrix as the cached matrix (if necessary), and returns the inverted 
## matrix

cacheSolve <- function(x, ...) {
  mat <- x$getInverse()
  if(!is.null(mat)) {
    message("Getting cached matrix")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setInverse(mat)
  mat

}

## Test case matrices

test_2x2 <- matrix(1:4, nrow = 2, ncol = 2)
test_3x3 <- matrix(c(1,2,3,0,1,4,5,6,0), nrow = 3, ncol = 3)
not_invertible <- matrix(1:9, nrow = 3, ncol = 3) # should fail bec not invertible
test_4x4 <- matrix(c(5,4,2,1,0,1,-1,-1,-1,-1,3,0,1,1,-1,2), nrow = 4, ncol = 4)


## Test functions on test cases

cacheSolve(makeCacheMatrix(test_2x2))
cacheSolve(makeCacheMatrix(test_3x3))
cacheSolve(makeCacheMatrix(test_4x4))
cacheSolve(makeCacheMatrix(not_invertible))



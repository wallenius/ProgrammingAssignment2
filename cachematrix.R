## Caching the inverse of a matrix
## Function makeCacheMatrix creates a cache matrix
## Function cacheSolve calculates the inverse of a makeCacheMatrix if
## not already cached.
##
## Example:
## A <- matrix(c(-1,1,2,1),2,2)
## Ac <- makeCacheMatrix(A)
## cacheSolve(Ac) # Inverse is calculated and cached
## cacheSolve(Ac) # Inverse is already cached and returned


## Function to create a cache matrix. Contains a list to
##   1: Set the matrix x
##   2: Get the matrix x
##   3: Set the inverse of the matrix x
##   4: Get the inverse of the matrix x

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

## cacheSolve caches the inverse of matrix x. If inverse is already
## cached, the function returns the cached inverse of matrix x,
## otherwise the inverse of matrix x is calculated, cached, and returned.
## cacheSolve accepts all arguments as solve().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

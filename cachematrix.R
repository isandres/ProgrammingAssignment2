## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## This assignment is to write a pair of functions that cache the inverse of a matrix.

## ---------------------------------------------------------------------------
## "makeCacheMatrix" function creates a special "matrix" object (x) that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(z) {
    x <<- z
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## ---------------------------------------------------------------------------------------------
## "cacheSolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  # Check if the inverse has already been calculated before
  if (!is.null(inv)){
    # get it from the cache and skips the computation. 
    return(inv)
  }
  
  # Compute the inverse otherwise
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # Set the value of the inverse in the cache
  x$setinv(inv)
  
  return(inv)

}

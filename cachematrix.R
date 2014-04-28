## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function holds the matrix data, the inverse. When required it 
# provides those two 
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) x_inv <<-inverse
  getinverse <- function() x_inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The function cacheSolve returns the inverse of a matrix A created with
## the makeCacheMatrix function.
## If the cached inverse is available, cacheSolve retrieves it and if it is
## not calculates caches and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinverse()
  if (!is.null(x_inv)) {
    message("getting cached inverse matrix")
    return(x_inv)
  } else {
    x_inv <- solve(x$get())
    x$setinverse(x_inv)
    return(x_inv)
  }
}

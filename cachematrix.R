## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  ## function that sets ta new matrix x and resets inverse as NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  ## getter
  get <- function() x
  ## setter of the inverse matrix with function solve
  setinverse <- function(inv) inverse <<- inv
  ## getter or the inverse
  getinverse <- function() inverse
  ##returned list
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## look for the inverse of the given matrix (list returned from makeCacheMatrix)
  inverse <- x$getinverse()
  ## if it exists already, just return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  ## otherwise, compute it, store it and return it
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}

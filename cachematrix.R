## This function allows to
## - set a new matrix, passed as a parameter x, and reset its inverse to null, and retrieve such matrix
## - set its inverse, passed as a prameter "inv", and retrieve it
## - return a list of all the above mentioned variables

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


## This function allows to
## - get the inverse of a matrix off a list x passed as parameter, which is supposed to be returned by the makeCacheMatrix function
## - check if it exists already, and in this case just return it as "cached data"
## - otherwise, compute it, store it in the list, and return it

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

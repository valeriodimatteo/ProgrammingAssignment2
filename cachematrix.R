## This function allows to:
## - A. set a new matrix, passed as a parameter x, and reset its inverse to null, and retrieve such matrix
## - B. set its inverse, passed as a prameter "inv", and retrieve it
## - C. return a list of all the above mentioned variables

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## A
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  ## B
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  
  ## C
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function allows to return a matrix that is the inverse of 'x'. In particular:
## - A. get the inverse of a matrix off a list x passed as parameter, which is supposed to be returned by the makeCacheMatrix function
## - B. check if it exists already, and in this case just return it as "cached data"
## - C. otherwise, compute it, store it in the list, and return it

cacheSolve <- function(x, ...) {
        
  ## A
  inverse <- x$getinverse()
  
  ## B
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## C
  data <- x$get()
  inverse <- solve(data,...)
  x$setinverse(inverse)
  inverse
}

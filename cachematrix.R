## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function create a "special" matrix which can have it inverse 
# cached for faster return when we want the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
#if the inverse of the matrix has already been calculated by a prior run of this function,
# we return the stored the inverse, otherwise we calculate the inverse and store for
# faster future recall
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

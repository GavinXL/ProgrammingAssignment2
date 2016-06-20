## The functions both calculate an inverse matrix
## and store prior matrix inversions in a cache
## so that they can be retrieved if already solved.

## makeCacheMatrix contains four functions
## that set the matrix, get the matrix, set the inverse
## and get the matrix within the cache

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x 
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set=set, get=get, setinv=setinv, getinv=getinv )
}


## cacheSolve takes the output of makeCacheMatrix,
## calculates the inverse, and returns the inverse to the cache

cacheSolve <- function(x,...) {
  i <- x$getinv()
  
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data,...)
  
  x$setinv(i)
  return(i)
}

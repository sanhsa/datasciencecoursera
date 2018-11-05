## Put comments here that give an overall description of what your
## functions do

## THIS FUNCTON CREATES A SPECIAL "MATRIX" OBJECT THAT CAN CACHE ITS INVERSE#
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv<- function(inverse) inv <<- inverse
  getinv<-function() inv
  list(set = set,
       get = get,
       setinv = setinverse,
       getinv = getinv)
}
## This function computes the inverse of the special matrix returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  return(inv)
}


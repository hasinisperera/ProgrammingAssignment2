## Put comments here that give an overall description of what your
## functions do

## Special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Computes the inverse of the special matric return by makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getinverse()
  if(!is.null(inv)) {
    message("")
    return(inv)
  }
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  x$setinverse(inv)
  return(inv)
}

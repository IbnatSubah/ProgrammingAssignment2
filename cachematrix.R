## These two functions will compute the inverse of a square invertible matrix if it is not 
## in cache. If it is already in cache and the matrix is not changed, then it will just
## return that cached valeu.

## This first function will:
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This next function should return the inverse of the matrix. It will first search if the
## inverse of the matrix is already available. If it is already available, it will just 
## return that but if it is not, it will compute the inverse.

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
  ## Return a matrix that is the inverse of 'x'
}

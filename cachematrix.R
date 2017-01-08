## makeCacheMatrix will cache the inverse of a matrix
## It will test for a square matrix

## get() will get the matrix of type makeCacheMatrix
## set() will set the matrix of type makeCacheMatrix and initialize its inverse
## getinverse() will get the inverse of the matrix
## setinverse() will set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  cachedinverse <- NULL
  set <- function (newx) {
    x <<- newx
    cachedinverse <<- NULL
  }
  get <- function() x
  setcachedinverse <- function(inverse) cachedinverse <<- inverse
  getcachedinverse <- function() cachedinverse
  
  list(set = set, get = get, setcachedinverse = setcachedinverse, getcachedinverse = getcachedinverse)
}


## cacheSolve will try to get the cached value of the inverse of the matrix
## otherwise it will calculate and set the inverse for the matrix

cacheSolve <- function(x, ...) {

## Gets the inverse of x
  cached <- x$getcachedinverse()
## Check to see if the inverse of matrix is NOT NULL
## If it's NOT NULL, return inv_x as retrieved above and ends cacheSolve function

  
  if (! is.null(cached)) {
    return(cached)
  }
## Inverse of matrix is NULL so calculate and set it
  m <- x$get()
  cached <- solve(m)
  x$setcachedinverse(cached)
  cached
}

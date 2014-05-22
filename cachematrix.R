## we create 2 functions here:
## 1. makeCacheMatrix(x) takes a square matrix x as input
## (note: we don't check for square conditions, just assume it is the case)
## and we associate to this matrix functions to get and set its value
## as well as get and set the value of its inverse
## this is a way to cache the value of x and its inverse for later use
## 2. cacheSolve(x) will check whether the inverse of x has already been computed
## and return the cached value if it is the case
## otherwise it calculates the inverse and caches it using makeCacheMatrix

## makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

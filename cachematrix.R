## The makeCacheMatrix function creates a list containing functions that:
## 1. set the value of a matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

## The cacheSolve function returns the inverse of the input matrix.
## It first tests to see whether the inverse is cached, and if so,
## returns the cached inverse.
## If the inverse is not cached, the inverse is calculated and returned.

## makeCacheMatrix: "set" and "get" functions for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve: returns the inverse of a matrix, using the cached inverse
##             if available

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

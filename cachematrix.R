## These functions cache the inverse of a matrix. To test it, use examples given in Len's thread: 
## "Simple test matrices for the lexical scoping programming assignment" from the Week 3 discussion board. 

## This function builds a set of 4 functions and returns them within a list to the parent environment. 
## It also initialises two objects, x and m, to use later. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}
## CacheSolve returns an matrix that is the inverse of 'x' USING THE SOLVE() function - inverse() is not a function. 

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
  
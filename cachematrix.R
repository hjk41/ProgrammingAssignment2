## This is a inverse matrix solver that caches the inverse so
## that we don't need to compute it over and over

## This function makes a wrapper over a matrix, so you can use
## it on cacheSolver
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function takes a wrapped matrix produced by makeCacheMatrix
## and computes the inverse of the matrix. It leverages caching
## so if the inverse is already computed, we just return the
## cached value
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


## testing the cache solver
cacheSolveTest <- function() {
  x = matrix(c(1,0,0,2), 2, 2)
  message("x is: ")
  print(x)
  xx = makeCacheMatrix(x)
  message("solving the first time: ")
  y = cacheSolve(xx)
  print(y)
  message("solving the second time: ")
  y = cacheSolve(xx)
  print(y)
}
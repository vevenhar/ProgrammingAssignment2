## Caching the Inverse of a Matrix

# Tests:
# x <- makeCacheMatrix()
# x$set(matrix(c(1, 2, 3, 4), nrow = 2, byrow = TRUE))
# x$get()
# x$setsolve(solve)
# x$getsolve()
# cacheSolve(x) -> getting cached matrix inverse

# y <- makeCacheMatrix()
# y$set(matrix(c(5, 6, 7, 8), nrow = 2, byrow = TRUE))
# y$get()
# cacheSolve(y) ->
#      [,1] [,2]
# [1,] -4.0  3.0
# [2,]  3.5 -2.5

# creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix using the solve function
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached matrix inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

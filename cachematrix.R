## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL  # Reset cached inverse when matrix is updated
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function calculates the inverse of the matrix, but first checks if it’s already cached. 
## If cached, it retrieves it; otherwise, it calculates the inverse and caches it.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## Test the functions
mat <- matrix(c(2, 4, 3, 1), 2, 2)  # Create a 2x2 matrix
cachedMatrix <- makeCacheMatrix(mat)  # Apply makeCacheMatrix
cacheSolve(cachedMatrix)  # First calculation
cacheSolve(cachedMatrix)  # Should use the cached result

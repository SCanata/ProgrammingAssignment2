## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. It takes a matrix as input and returns a list of functions that access and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse to NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate existing inverse
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the "matrix" returned by makeCacheMatrix. It checks if the inverse is already cached and returns it to avoid redundant calculations; otherwise, it computes the inverse, caches it, and then returns it.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Try to get the cached inverse
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  data <- x$get()  # Get the original matrix
  inv <- solve(data)  # Compute the inverse
  x$setInverse(inv)  # Cache the inverse
  inv
}
# Create a square matrix
A <- matrix(c(2, 3, 1, 4), nrow = 2, ncol = 2)

# Create the special "matrix" object
cachedA <- makeCacheMatrix(A)

# Compute and cache the inverse
inverseA <- cacheSolve(cachedA)
print(inverseA)

# Retrieve the cached inverse without recomputing
inverseA2 <- cacheSolve(cachedA)
print(inverseA2)


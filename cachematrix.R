## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset the inverse property when the matrix is updated
  }
  get <- function() x  # Retrieve the matrix
  setinverse <- function(inverse) inv <<- inverse  # Set the inverse of the matrix
  getinverse <- function() inv  # Retrieve the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # Check if the inverse is already cached
  if(!is.null(inv)) {  # If cached, return the cached inverse
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # Retrieve the matrix from the special "matrix" object
  inv <- solve(data, ...)  # Compute the inverse of the matrix
  x$setinverse(inv)  # Cache the computed inverse
  inv  # Return the inverse
}


## Usage Example

# my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), 2, 2))
# inverse_matrix <- cacheSolve(my_matrix)
# cached_inverse <- cacheSolve(my_matrix)

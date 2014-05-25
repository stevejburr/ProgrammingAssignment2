## This code has two functions, the first "makeCacheMatrix" creates a "special matrix"
## which can cache its inverse.
## The second "cacheSolve" checks whether the inverse of the "special matrix" has
## already been calculated and returns it if it has. If not it calculates it, returns the value
## and stores it in the cache.

## This function creates a list which:
# 1. Sets the value of the matrix
# 2. Gets the value of the matrix
# 3. Sets the value of the inverse of the matrix
# 4. Gets the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function checks if the inverse of the matrix has already been calculated
# If it has then this value is returned
# If not then the inverse of the matrix is calculated, cached and returned

cacheSolve <- function(x, ...) {
  # This step is checking if the inverse is already calculated and returns if it has
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # In the event that it haasn't been calculated then it is calculated and cached
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


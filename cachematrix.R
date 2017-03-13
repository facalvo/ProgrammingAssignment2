#Programming assigment 2

# Matrix inversion is usually a costly computation and there may be some 
# benefit to caching the inverse of a matrix rather than computing it 
# repeatedly. The following equations aim at solving this problem


# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #Instead of mean, inverse
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

#This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
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

# example for checking the functions 
Mtx <- matrix(c(8, 6, 3, 1), 2, 2)
my_matrix <- makeCacheMatrix(Mtx)
my_matrix$get()

my_matrix$getinverse()
cacheSolve(my_matrix)
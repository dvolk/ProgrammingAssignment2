## This is an implementation of a matrix type that caches its
## inverse

## makeCacheMatrix(X) takes a matrix and creates a matrix that
## caches its own inverse. It does this by storing the matrix
## and the inverse in a list
makeCacheMatrix <- function(X = matrix()) {
  m <- NULL
  set <- function(y) {
    X <<- y
    m <<- NULL
  }
  get <- function() X
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve(X) takes a matrix made by makeCacheMatrix and
## returns the cached inverse if it exists, or computes
## it and caches it in X
cacheSolve <- function(X) {
  m <- X$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- X$get()
  m <- solve(data)
  X$setinverse(m)
  m
}

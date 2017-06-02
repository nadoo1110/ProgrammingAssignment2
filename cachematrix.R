## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix() caches the inverse of a matrix,
## cacheSolve() solves the inverse of a matrix using either solve or it uses the cached value 
## using makeCacheMatrix()

## Write a short comment describing this function
## makeCacheMatrix creates a matrix, sets the value of the matrix, return a  matrix, set and get
## the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv<<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## cacheSolve() returns the inverse of a matrix, if the inverse is already found, it will 
## get the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}





##It contains a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  I = matrix()
  set <- function(y) {
    x <<- y
    I<<- matrix()
  }
  
  get <- function() x  
  getInverse<- function() I
  setInverse<- function(inverse) I<<- inverse

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated (and the matrix has not changed)
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  x$setInverse(I)
  I
}

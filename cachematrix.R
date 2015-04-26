##It contains a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #stores inverse value
  #Initialised to NULL
  I <- NULL
  #creates matrix
  set <- function(y) {
    x <<- y
    I<<- NULL
  }
  # get the matrix
  get <- function() x  
  
  #get inverse matrix from I
  getInverse<- function() I
  
  setInverse<- function(inverse) I<<- inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

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
  matrix <- x$get()
  I<- solve(matrix)
  x$setInverse(I)
  I
}

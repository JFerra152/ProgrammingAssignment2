## The below two functions are to cache the inverse of a matrix

## This function will create a special matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Inverse being initialized
  j <- NULL
  
  ## Setting the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  
  ## Getting and setting the matrix inverse
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  
  ##Return a list of the methods
  list(set = set, get = get,
  setInverse = setInverse,
  getInverse = getInverse)
}

## Function to get the inverse of the returned special matrix
## This function will capture the inverse if it is already present as well
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  
  ## Returns the inverse if it is already set
  if(!is.null(j)) {
  message("getting cached data")
  return(j)
  }
  
  ## Retrieve the matrix from object
  mat <- x$get()
  
  ## Calculate the inverse
  j <- solve(mat, ...)
  
  ##Set the inverse of the object
  x$setInverse(j)
  
  ## Return the matrix
  j
}

###################################################
## This function caches all input/output values
## It provides setter/getter functions that
## updates and retrieves cached values
###################################################
makeCacheMatrix <- function(x = matrix()) {
  ## Initialization
  inv <- NULL
  
  ## Setter function that updates the input
  set <- function(y) {
    x <<- y
    ## update the inverse to NULL once input changes
    inv <<- NULL
  }
  
  ## Getter function to retrieve input matrix
  get <- function() {
    x
  }
  
  ## Setter function that updates cached inverse matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## Getter function that returns cached inverse matrix
  getInverse <- function() {
    inv
  }
  
  ## Returning a list of functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

###################################################
## This function inverses an input matrx
## It first checkes if an inverse already exists
## and returns the caching inverse if exists.
## Otherwise it does a fresh inverse of the matrix
###################################################
cacheSolve <- function(x, ...) {
  ## Getting existing inverse of matrix x
  inverse <- x$getInverse()
  
  if (!is.null(inverse)) {
    ## If inverse already exists, return existing one
    message("Inverse already exists. Returning the existing one")
    return(inverse)
  }
  
  ## Getting input matrix that's to be inversed
  data <- x$get()
  ## Inversing the matrix
  inverse <- solve(data)
  ## Updating inverse in cache
  x$setInverse(inverse)
  ## Return inverse matrix
  inverse
}
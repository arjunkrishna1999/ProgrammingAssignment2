## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  j <- NULL
  ## Method to set the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  ## Method the get the matrix
  get <- function()x
   ## Method to set the inverse of the matrix
  setInverse <- function(inverse) j <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  ## Just return the inverse if its already set
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  ## Get the matrix from our object
  mat <- x$get()
   ## Calculate the inverse using matrix multiplication
  j <- solve(mat,...)
  x$setInverse(j)
  j
}

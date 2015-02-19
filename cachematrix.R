## This pair of functions allow the solution to a matrix to be cached
## and retrieved when needed.  In the case the base matrix is reset, the
## cached inverse is nulled out

## this function is the cache mechanism

makeCacheMatrix <- function(baseMatrix = matrix()){
  ## create inverse matrix in global environment--initialize it to null
  inverse <<- NULL
  set <- function(newMatrix){
    ## when we reset, capture the matrix and clear the inverse
    message("clearing cached data")
    baseMatrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() baseMatrix  ## return the base matrix
  setInverse <- function(solvedMatrix) {
      ## after calculating the inverse matrix, save it
      message("caching solved matrix")
      inverse <<- solvedMatrix
  }
  getInverse <- function() inverse
  ## return a list with these parts
  list(get = get, set = set, setInverse = setInverse, getInverse = getInverse)
}


## this function refreshes the cached matrix if a new matrix
## has been created and returns the solved matrix if available

cacheSolve <- function(x, ...) {
  ## retrieve inverse from global env
  i <- x$getInverse()
  if (!is.null(i)){
    ## if inverse is cached, return it
    message("getting cached data")
    return(i)
  }
  ## else get the base matrix and solve for the inverse
  baseMatrix <- x$get()
  i <- solve(baseMatrix)
  ## cache the result
  x$setInverse(i)
  i    ## return the inverse matrix
}

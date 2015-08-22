## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
  i <- NULL
  set <- function(y)
  {
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(solve) i <<- solve
  getInverseMatrix <- function() i
  
  list(set = set , get = get , setInverseMatrix = setInverseMatrix , getInverseMatrix = getInverseMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i<-x$getInverseMatrix()
  if(!is.null(i))
  {
    message("getting cached data")
    return(i)
  }
  #message("Not cached")
  matrix <- x$get()
  i <- solve(matrix, ...)
  x$setInverseMatrix(i)
  i
}

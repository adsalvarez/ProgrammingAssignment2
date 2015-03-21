## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The Funcion makes list  contain the matrix with funcions get, set, getMatrix and setMatrix
## when the  set function is called the m (inverse matrix) variable is clean
## wich allows function cacheSolve tests if a inverse matrix  has already been calculated

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve
  getMatrix <- function() m
  
  list(set = set, 
       get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix
       )

}


## This function tests if x already been if the matrix in getmatrix is null, 
## then cacheSolve Calculates de inverse of the parameter X, else Gets the matrix 
## already calculated 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)

  x$setMatrix(m)
  
  m
}

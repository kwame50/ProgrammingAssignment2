## The purpose of this assignment is to create 2 functions that cache the inverse of a matrix. 
## 1. makeCacheMatrix function
##    - set the value of the matrix
##    - get the value of the matrix
##    - set the value of matrix inversion
##    - get the value of matrix inversion
## 2. cacheSolve function
##    - computes the inverse of a square matrix
##    - if the inverse has already been calculated, the function retrieves the inverse from the cache.

## makeCacheMatrix is a function that creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m_inverse <- NULL
  set <- function(y){
    x <<- y
    m_inverse <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m_inverse <<- solve
  getmatrix <- function() m_inverse
  list(set = set, get=get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}



## cacheSolve is a function that computes the inverse of the matrix returned by the makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  m_inverse <- x$getmatrix()
  if(!is.null(m_inverse)){
    message("getting cached data")
    return(m_inverse)
  }
  matrix <- x$get()
  m_inverse <- solve(matrix, ...)
  x$setmatrix(m_inverse)
  m_inverse
}


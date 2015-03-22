## The purpose of this assignment is to cache the inverse of a matrix. 
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
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}



## cacheSolve is a function that computes the inverse of the matrix returned by the makeCacheMatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}


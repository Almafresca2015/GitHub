## Put comments here that give an overall description of what your
## functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x=matrix()){
  inverse_ <- NULL
  setmatrix <- function(y){
  matrix <<- y
  inverse_ <<- NULL
  }
  getmatrix <- function(){
    matrix
  }
  setinversematrix <- function(inverse) {
    inverse_ <<- inverse
  }
  getinversematrix<- function() {
     inverse_
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inverse_ <- x$getinversematrix()
  if(!is.null(inverse_)) {
    message("getting cached data")
    return(inverse_)
  }
  data <- x$getmatrix()
  m <- solve(data) %*% data
  x$setinversematrix(m)
  
  m 
}
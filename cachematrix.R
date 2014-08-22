## Functions to compute the inverse of a matrix or retrieve the inverse if cached, for
## ProgrammingAssignment2 of R Programming of Coursera.
## makeCacheMatrix defines the structure to store or retrieve the matrix and the inverse
## of a matrix
## cacheSolve computes the inverse of a given matrix if it hasn't bee compute before, in that case
## the inverse is retrieved 


## Gives the structure to store and retrieve the matrix and the inverse matrix, so for 
## a given cached matrix m it can be retrieved with m$get() or modify with m$set(newmatrix) and for  
## its inverse m$getInverse() to retrieve and m$setInverse(inverse) to set or modify 
makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(matriz) {
    m <<- matriz
    i <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set=set, get=get,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of a cached matrix (it means previously created with 
## makeCacheMatrix),but if the inverse was already computed with cacheSolve, then the inverse
## was already stored and it just retrieves it
cacheSolve <- function(m, ...) {
  inv <- m$getInverse()
  if(!is.null(inv)) {
    message("retrieving cached data")
    return(inv)
  }
  data <- m$get()
  if(nrow(data)!=ncol(data))
  {
    message("the matrix is not square cannot compute inverse")
  }
  else
  {
  inverse <- solve(data, ...)
  m$setInverse(inverse)
  inverse
  }
}

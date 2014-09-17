## The following two functions: makeCacheMatrix and cacheSolve offer a way of
## cacheing the inverse of a numeric or complex invertible square matrix in
## R's global environment. It can speed things up by sparing the resource
## intensive process of re-inverting a matrix every time it's needed.

## makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse. It's argument must be an invertible numeric or complex matrix.
## It outputs a list of functions that allow the user to set and retrieve the
## values of the matrix and its inverse.

makeCacheMatrix<- function (x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) im <<- inverse
  getinverse <- function() im
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from
## the cache.
 
cacheSolve<- function (x,...){
  im <- x$getinverse()
  if(!is.null(im)) {
    message("getting cached data")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
## This file contains two functions that create a special matrix and
## cache the inverse of the matrix, so that it doesn't need to be
## recalculated everytime.
## The first function "makeCacheMatrix" creates the special matrix
## and the second function "cacheSolve" computes or retrieves the
## inverse of the matrix.

## this function creates a special matrix capable of 
## caching the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  #create "inverse" variable and set to NULL
  inverse <- NULL
  
  # setter f(x) to set the matrix and initialize inverse
  set <- function(mat) {
    x <<- mat
    inverse <<- NULL
  }
  
  # getter f(x) to return the special matrix object
  get <- function() x 
  
  # setter f(x) to set the inverse variable
  setinv <- function(inv) inverse <<- inv 
  
  # getter f(x) to return the inverse variable
  getinv <- function() inverse
  
  # return the special matrix as list
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## The following function computes the inverse of a "special" matrix
## of the type created by the MakeCacheMatrix function (see above)

cacheSolve <- function(x, ...) {
  
# retrieve the inverse from the matrix passed to the function
  inv <- x$getinv()
  
  # if the inverse matrix was present and retrieved,  then
  # print "getting cacheed data" message and return the inverse
  # matrix.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    }
  
  # if inverse matrix was not present, then calulate the inverse
  # matrix with solve() and store in the special matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}


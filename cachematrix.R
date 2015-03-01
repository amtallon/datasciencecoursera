## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invx <- null
  set <- function(y){
    x <<- y
    invx <<- null
    
  }
  get <- function() x
  setmatrix <- function(solve) invx <<- solve
  getmatrix <- function() invx
  list(set = set, get = get, 
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function
#This function computes the inverse of the special matrix returned by the 
#makeCacheMatrix function.
#If the inverse has already been calculated (and the matrix has not changed),
#this function should retrieve the inverse from the cache,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invx <- x$getmatrix()
  if(!is.null(invx)){
    message("cached inverse matrix")
    return(invx)
  }else {
    matrix <- x$get()
    invx < - solve(matrix, ...)
    x$setmatrix(invx)
    return(invx)
  }
}

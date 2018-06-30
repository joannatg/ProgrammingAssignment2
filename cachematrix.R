## function creates mattirx obj, sets it`s value, gets that value and sets value of it`s inverse, and gets that value

makeCacheMatrix <- function(x = matrix()) {  
   IM <- NULL
  set <- function (y) {
    x <<- y
    IM <<- NULL
  }
  get <-function() x
  setMatrix <- function(inv) IM<<- inv
  getMatrix <- function() IM
  list (set=set, get=get, setMatrix=setMatrix, getMatrix=getMatrix)
}


## function gets output of makeCacheMatrix(matrix) and checks the value of the inverse of the matrix
## if there is value, it prints Retreiving cached data and return the inverse value  
## if no value (empty), it gets initial data and solves the inverse value of matrix

cacheSolve <- function(x, ...) {
 IM <- x$getMatrix()
  if(!is.null(IM)){
    message("getting cached data")
    return(IM)
  }
  cd <- x$get()
  IM <- solve(cd,...)
  x$setMatrix(IM)
  IM
}

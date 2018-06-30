##function sets value of matrix, gets value of matrix, sets it`s inverse, and gets that value
makeCacheMatrix <- function(x = matrix()) {
  
  IM <- NULL
  set <- function(y) {
    IM <<- y
    IM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) IM <<- inverse
  getInverse <- function() IM
  list(set = set,
       get = get,
       setInverse = setInverse ,
       getInverse = getInverse)
  
}
## function gets output of makeCacheMatrix(matrix) and checks the value of the inverse of original matrix
## if no value (empty) it gets initial matrix data and sets inverse matrix
## if there is value, it get the value (of inverse matrix) and returns Retreiving cached data

cacheSolve <- function(x, ...) {
  IM <- x$getInverse()
  if(!is.null(IM)) {
    message("Retreiving cached data.")
    return(IM)
  }
  data <- x$get()
  IM<- solve(data)
  x$setInverse(IM)
  IM
}

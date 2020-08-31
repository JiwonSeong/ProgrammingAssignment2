## The program calcualtes inverse of inputted matrix, if the matrix is invertible. 
## If the matrix was inverted or used before, it will output the cached data.

## makeCacheMatrix function operates pretty much in the same way as the make_Vector 
## function from the task example: it creates 'special' matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <-function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inverse_matrix <<- inverse
  getinv <- function() inverse_matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix from the first 
## function, if its invertible matrix. All other part is same as from the example.

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinv()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  if(det(data, ...) == 0) {
    inverse_matrix <- "There is no inverse as the determinant of the matrix equals to zero"
    x$setinv(inverse_matrix)
    return(inverse_matrix)
  }
  inverse_matrix <- solve(data, ...)
  x$setinv(inverse_matrix)
  inverse_matrix
}


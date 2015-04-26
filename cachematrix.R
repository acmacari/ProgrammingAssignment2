## makeCacheMatrix and cacheSolve cache the inverse of a matrix so that 
## it does not compute it repeatedly, which can be costly.

## makeCacheMatrix creates a list to set the value of the matrix, get that 
## value, set the value of the inverse and get the value of that inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { ##Creates a function that changes the matrix in main function
    x <<- y
    m <<- NULL
  }
  get <- function() x ##returns the matrix x in the main function
  setinverse <- function(inverse) m <- inverse ##set the inverse matrix
  getinverse <- function() m #get the inverse matrix
  list(set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##cacheSolve skips inverse computation if it has already been computed. If not,
##it computes the inverse and set the value in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ##Checks to see if inverse has already been calculated and stored
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ##if so, return inverse
  }
  ##If not, get the matrix object
  data <- x$get()
  m <- solve(data, ...) ##calculate inverse
  x$setinverse(m) ##set inverse to the object
  m ##return result
}

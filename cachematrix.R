## makeCacheMatrix and cacheSolve cache the inverse of a matrix

## Stores a list of functions
## set and get the value of the matrix, and set/get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <- inverse
  getinverse <- function() m
  list(set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Checks to see if inverse has already been calculated
## If so, get inverse from the cache 
## If not, calculate inverse and set value of the inverse in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

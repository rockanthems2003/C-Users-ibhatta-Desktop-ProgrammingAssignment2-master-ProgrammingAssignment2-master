## this function is setting x as a global variable so that it can be accessed outside of the function within which it is created.
## m is assigned NULL here, but we will input the value of the inverse matrix into m.
##The input to the makeCacheMatrix function can only be a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##we are assinging the value of the matrix to the variable called data.
## We stored the inversed matrix into the variable m.
## cachesolve returns m (inversed matrix) as the return value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}





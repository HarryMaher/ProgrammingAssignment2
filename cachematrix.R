## Since matrix inversion can be costly computationally, the following two
## functions work to cache a matrix to save memory.

## makeCacheMatrix creates a list containing a function to: 
# 1. set matrix value
# 2. get matrix value
# 3. set inverse matrix  value
# 4. get inverse matrix value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve returns inverse of matrix. 
## if it's already inversed, it returns the cached inversed m matrix along with a pleasant message.
## If it's not inversed, it finds the inverse and puts it in the cache for later.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting dat cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}

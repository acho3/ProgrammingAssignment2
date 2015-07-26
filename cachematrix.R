## Similar to "Example: Caching the Mean of a Vector.
## This one will store a matrix and cache its inverse.

## This function creates a special "vector", which is
## a list containing a function to
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the inverse
##  4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m <<- inverse
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Find the inverse of the matrix from the above function.
## It first checks to see if the inverse has been calculated
## If so, it will get the inverse from the cache and skip the computation
## Otherwise, it calculates the inverse and sets the inverse via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinverse()
      if(!is.null(m)){
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m
}

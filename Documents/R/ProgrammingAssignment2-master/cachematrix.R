## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    get <- function() x
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    
    getinv <- function() inv
    setinv <- function(inverse) inv <<- inverse
    
    list(get=get, set=set, getinv=getinv, setinv=setinv)
  }
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  
  if (!is.null(inv)) {
    message("inverse is cached")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  
  x$setinv(inv)
  
  return(inv)
}

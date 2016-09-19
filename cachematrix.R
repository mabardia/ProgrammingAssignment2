## Put comments here that give an overall description of what your
## functions do
## Given a invertible matrix, the following two functions will
## calculate the inverse matrix or retrieve the inverse matrix 
## from the cache.

## Write a short comment describing this function
## define() is a function that changes vector in the main function
## toma() is a function that returns vector x in the main function
## define_inverso() & toma_inverso() are functions within main that
## do the storage of the vector value and invert it


makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      define <- function(y) {
            x <<- y
            m <<- NULL
      }
      toma <- function() x
      define_inverso <- function(solve) m <<- solve
      toma_inverso <- function() m
      list(define = define, toma = toma,
           define_inverso = define_inverso,
           toma_inverso = toma_inverso)
}


## Write a short comment describing this function
## Function “cacheSolve” computes the inverse of the special “matrix” 
## (which is the input of cachemean) returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not 
## changed), then the cachesolve should retrieve the inverse from the 
## cache. If the inverse has not been calculated, data gets the matrix 
## stored with makeCacheMatrix, m calculates the inverse, and x$toma(m)
## stores it in the object m in makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$toma_inverso()
      if(!is.null(m)) {
            message("obteniendo cache")
            return(m)
      }
      data <- x$toma()
      m <- solve(data, ...)
      x$define_inverso(m)
      m
}

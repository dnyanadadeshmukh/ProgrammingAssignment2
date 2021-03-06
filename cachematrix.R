## makeCacheMatrix funcion is created to define get and set functions used to get or set value of a matrix from cache.
## set function would set the input value in cache
## get funcion would get the value of matrix x
## getInverse is used to get the inverse of a matrix set in cache in m
## setInverse is use set the inverser of a matrix in m

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  ## set the inverse in cache variable m
  setInverse <- function(inverse) m <<- inverse
  ## get the inverse of matrix from m
  getInverse <- function() m
  ## return the list of functions 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve function is used to get the inverse of a matrix using Solve function in R
## cacheSolve function looks for inverse of input matrix and if it is available in cache, it retrieves that
## if inverse of given matrix is not available in cache, it calculates inverse using solve() and then stores it in cache.

## x is input matrix which is a square and invertible matrix

cacheSolve <- function(x, ...) {
        
        ## calling getInverse on iput object x to see if inverse is available in cache
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if inverse is not available in cache get the input matrix and compute its inverse
        data <- x$get()
        m <- solve(data, ...)
        ## set the computed inverse of a matrix in cache
        x$setInverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
}







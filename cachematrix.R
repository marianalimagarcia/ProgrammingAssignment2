# A pair of functions that cache the inverse of a matrix

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    message("matrix created")
  }
  get      <- function() x
  setSolve <- function(solve) inv <<- solve
  getSolve <- function() inv
  list(Set = set, 
       Get = get,
       SetSolve = setSolve,
       GetSolve = getSolve)
}

## Return a matrix that is the inverse of 'x'(only if the matrix is square). If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$GetSolve()
  if(!is.null(m)) {
    message("getting cached data of matrix")
    return(m)
  }
  if(dim(x$Get())[1]==dim(x$Get())[2]){
    m <- solve(x$Get(), ...)
    x$SetSolve(m)
    m 
  }else{
    message("This matrix is not square")
  }
}
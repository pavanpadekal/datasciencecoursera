## R Code for caching the inverse of a matrix which is a computationally intensive operation
## This code fetches the inverse of a matrix from cache if the input i.e., the input matrix has not changed. 
## Else it calculates it
## The code comprises of 2 functions. makeCacheMatrix and cacheSolve


## This is a function that returns a special vector of functions, i.e., set, get, setmatrix and getmatrix
## They essentially return functions that either initialize or fetch the input matrix/inverse matrix respectively
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

## This function essentially decides if the input matrix's inverse is available in cache 
## if avaialble fetches it from cache else calculates it 
cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix, ...)
  x$setmatrix(m)
  m
  ## Return a matrix that is the inverse of 'x'
}
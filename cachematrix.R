## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##With this function we can set an special matrix, get the matrix and get its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){ 
    x <<- y
    inv <<- NULL
  }
  
  get <- function() (x)
  setInver <- function(inver) {inv <<- inver}
  getInver <- function() {inv}
  list(set = set, get = get, setInver = setInver, getInver = getInver)
}


## Write a short comment describing this function
##With this function we can calculate the inverse of the special matrix
##If the inverse of the same special matrix was already calculated, 
##we can get it from the cache
cacheSolve <- function(x, ...) {
  inv <- x$getInver()
  if(!is.null(inv)){
    message("we are getting cache")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInver(inv)
  inv   ## Return a matrix that is the inverse of 'x'
}

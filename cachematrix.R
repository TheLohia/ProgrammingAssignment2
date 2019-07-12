## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setmean = setmean, getmean = getmean)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    if(!is.null(inverse))
    {
      message('Retrieving cached data...')
      return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinv(inverse)
    inverse
}

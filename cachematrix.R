## Functions to cache a matrix an its inverse os as to save time and avoid calcualting inverse multiple 
## times

## Stores matrix as well as provides certain functions to store and retrieve matrix and its inverse

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


## Returns the inverse. Calculates the inverse if it has not been calculated and stores it as cached m/m

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

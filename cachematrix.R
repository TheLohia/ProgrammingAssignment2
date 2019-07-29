### Note to evaluator - incase of any errors, kindly let me know the specific error the feedback feature

## Functions to cache a matrix an its inverse os as to save time and avoid calcualting inverse multiple 
## times

## Function to store matrix and its inverse as cache
## It also provides methods to store and retrieve matrix and its inverse
## inv is variable which stores the inverse of matrix x
## get method returns the matrix
## set method is used to input a matrix (passed as argument) into x
## getinv method returns the inverse of matrix x
## setinv method stores the inverse of matrix x (passed as argument) in inv

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


## Function to calculate the inverse of a matrix and store it in cache memory in the makeCahceMatrix object

cacheSolve <- function(x, ...) {
    ## Retrive the inverse of 'x'
    inverse <- x$getinv()
    ## If inverse exists, the if clause will retrieve the inverse and return it
    if(!is.null(inverse))
    {
      message('Retrieving cached data...')
      return(inverse)
    }
    ## If the inverse does not exist, the next clause will calculate the inverse, store it in makeCacheMatrix object and return it
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setinv(inverse)
    inverse
}

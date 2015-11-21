## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly 
## The assignment is to write a pair of functions that cache the inverse of a matrix


makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  inv = NULL
  
  set = function(y) {
    # use `<<-` to assign a value to an object in an environment different from the current environment. 
    # <<- used for assigning variables in the parent evnt, more like global variable assignment. 
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inverse) inv <<- inverse 
  
  getinv <- function() inv
  
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## to use the functions stored in the main function, you need to subset the main function.
## To do this, you need the name of the main function + $ + the name of the second function + (arguments) 

cacheSolve <- function(x1, ...) {
  ## x1: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  inv1 <- x1$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(inv1)){
    # get it from the cache and skips the computation. 
    message("getting cached data")
    return(inv1) # return(inv1) statement will end the entire cacheSolve function.
  }
  
  # otherwise, calculates the inverse.  
  #  Because of the earlier return statement, everything else (the below 3 lines) then becomes the else {} statement.
  data <- x1$get()
  inv1 <- solve(data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x1$setinv(inv1)
  inv1
}
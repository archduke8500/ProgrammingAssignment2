## This set of functions allows for caching of 
## the inverse of multiple matrices for 
## later or repeated access without recalculating

## For any matrix, the makeCacheMatric function 
## caches the inverse variable and outputs
## a list of vectors to set the matrix, get 
## the matrix, calculate the inverse and 
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Sets Inverse to null, it is not calculated until needed
  Inv <- NULL
  # The set function allows for later assignment of the 
  # matrix for which the inverse is to be calculated
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  # The get function returns the matrix x, the setInverse function 
  # caches the inverse, and the getInverse function 
  # returns the cached inverse
  get <- function() x
  setInverse <- function() Inv <<- inverse
  getInverse <- function() Inv
  list(set=set, get=get, setInverse=setInverse,
       getInverse=getInverse)
}


## For any matrix, this function uses the output of functions
## from the makeCacheMatrix and attempts to get a previously 
## calculated inverse (if it exists). Otherwise it calculates the
## inverse and sets it in the cache for future use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInverse()
  # Return the inverse only if it is not null and exit
  if (!is.null(Inv)){
    message("getting cached data")
    return(Inv)
  }
  # Else calculate the inverse using solve and cache it for
  # further use using setInverse
  data <- x$get()
  Inv <- solve(data,...)
  x$setInverse(Inv)
  Inv
}

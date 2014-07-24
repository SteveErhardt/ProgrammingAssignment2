## This file contains 2 functions makeCacheMatrix() and cacheSolve()
## The overall functionality of these files is to cache and return the value of a Matrix, create 
## it's inverse and to cache and return the value of the inverse. The individual functions are 
## documented in line

## makeCacheMatrix takes a matrix as an argument. It defines 4 "sub" functions to "set" the matrix, 
## get the matrix, set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {

  ## set the inv variable to NULL in case makeCacheMatrix has been run on other data previously,
  ## this will ensure that cacheSolve will calculate the inverse when run and not just return an 
  ## old cached value
  inv <- NULL
  
  ## The set function caches the matrix
  set <- function(y) {
    ## give x the value that is passed to the function
    x <<- y
    ## reset inv whenever set is run
    inv <<- NULL
  }
  
  ## get is a function that returns the value of X (the matrix)
  get <- function() x
  
  ## setinverse is a function the takes the passed argument and assigns it to variable inv 
  ## (the inverse of the matrix once it's been calculated)
  setinverse <- function(inv.var) inv <<- inv.var
  
  ## getinverse is a function that returns the stored variable inv (this 
  ## is the inverse of the matrix once it's been calculated)
  getinverse <- function() inv
  
  ##The next bit creates a list containing the 4 functions -> set, get, setinverse, getinverse
  ##This list will be referenced by cacheSolve()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve tests to see if the inverse has already been calculated, if it has it will return 
## the value of the cache. If not, it will calculate the inverse and cache it using "setinverse"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Assign the variabe 
  inv <- x$getinverse()
  ## If inv has a value ( the inverse has been previously calculated) then 
  ## get the data from the cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## If inv is still NULL ( the inverse hasn't previously been calculated) then 
  ## calculate the inverse and cache the answer
  
  ## get the matrix
  data <- x$get()
  ## calculate the inverse
  inv <- solve(data, ...)
  ##cache the answer
  x$setinverse(inv)
  ## return the answer
  message("No cached data so calculating")
  return(inv)
}

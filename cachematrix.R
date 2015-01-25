## The functions bellow will calculate the inverse of a matrix 
# only if it was not calculated before. 

## Function makeCacheMatrix creates a list containing functions 
# to set the value of the matrix, get the value of the matrix, 
# set the value of the inverse, and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cahceSolve checks if the inverse was calculated 
# already; if yes, then it returns the inverse, if not, then 
# it calculates the inverse using buil-in "solve" function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv<- solve(data, ...)
  x$setinverse(inv)
  inv
}

## Matrix Caching Functions.
## The functions defined in this file enable the caching the inverse of a matrix,
## rather than compute it each time the inverse is required.

##This function creates a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  # define available utility functions...
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


#This function calculates the inverse of a given 'makeCacheMatrix' object, if an only if 
#the inverse has not already been camputed. If the inverse already exists, then it retrieves
# the last computed value.
cacheSolve <- function(x, ...) {
  i <- x$getinverse() # get the inverse of x (if it exists...)
  if(!is.null(i)) {# if the inverse is not null
    message("retrieving cached inverse data")
    return(i) #return the previously computed value.
  }
  # if the inverse is null...
  data <- x$get() # get the matrix data.
  i <- solve(data, ...) # compute the matrix inverse.
  x$setinverse(i) # assign the result to the relevant makeCacheMatrix variable.
  i #return the inverse to the user.
}

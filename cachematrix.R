##  The two functions below jointly calculate the inverse of a matrix. 
## and make it available to the cache environment.
## If the matrix has been previously cached the inverse will be 
## returned without repeated calculations.


## makeCacheMatrix creates a special matrix wich contains.
## a. sets the value of a matrix, b. gets the value of a matrix.
## d. sets the inverse of the matrix e. gets the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cachSolve obrtains teh inverse of the matrix created in makeCacheMatrix
## It checks to see if the inverse of the matrix has been previously obtained 
## and and has not changed and has been cached. If so it obtains the inverse from cache and does not recalculate it.
## Else, it calculates the inverse and sets it in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}







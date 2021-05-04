## Put comments here that give an overall description of what your
## functions do

## The "makecachematrix" is a function that creates a matrix which is able to
## cache its inverse. this function sets the value of the matrix and the inverse 
##and gets the value of the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL 
  }
  get <- function() {x}
  setinverse <- function(inverse) {inv <<- inverse}
  getinverse <- function() {inv}
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}



## the cache solve function computes the inverse of the matrix if it has not been calculated
## if the inverse has been calculated it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
       if(!is.null(inv)){
         message("getting cached data")
          return(inv)
         
       }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv
}

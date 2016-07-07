## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix creates a special "matrix", which is
##really a list containing a function to
#1.set the value of the matrix.
#2.get the value of the matrix.
#3.set the value of the inverse.
#4.get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
s <- NULL
set <- function(y){
  x <<- y
  s <- NULL
} 
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set=set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
##cacheSolve calculates the inverse of the special "matrix"
##created with the above function. However, it first checks to
##see if the inverse has already been calculated. If so, it gets the
##inverse from the cache and skips the computation. Otherwise, it calculates
##the inverse of the data and sets the value of the inverse
##in the cache via the setmean 

cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
  
     ## Return a matrix that is the inverse of 'x'
}

#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 

#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#This function, makeCacheMatrix creates a special "Metrix", which is really a list containing a function to
#set the value of the Matrix
#get the value of the Matrix
#set the value of the Inverse
#get the value of the Inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x,...){
  m <- x$getInverse()
  if(!is.null(inv)){
    print("Getting cached Matrix data")
    x$getInverse
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
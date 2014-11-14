## the makeCacheMatrix creates a "special" matrix that can be used to cache the inverse 
##of any matrix. 
##The cacheSolve function is used to find the inverse of a matrix
##if the inverse of the matrix is in the cache, the inverse is returned
##otherwise, the inverse is calculated

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) { 
  i<- NULL
  set <- function(matrix){
    x <<- matrix
    i <<- NULL
  }

  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}

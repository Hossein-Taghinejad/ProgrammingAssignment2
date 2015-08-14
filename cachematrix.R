## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
##1.set the value of the vector
##2.get the value of the vector
##3.set the value of the mean
##4.get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
      Inv <- NULL
      set <- function(y) {
            x <<- y
            Inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) Inv <<- inverse
      getinverse <- function() Inv
      list(set = set, get = get,setinverse = setinverse, getinverse = getinverse)
}


##cacheSolve calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has 
##already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets 
##the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
      Inv <- x$getinverse()
      if(!is.null(Inv)) {
            print("getting cached data")
            return(Inv)
      }
      data <- x$get()
      Inv <- solve(data, ...)
      x$setinverse(Inv)
      Inv
}

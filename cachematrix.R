## The functions below can be used to calculate the inverse of a matrix
## and cache the result so that it can be retrieved later to save computing time.


## The makeCacheMatrix funtion creates a special kind of matrix

makeCacheMatrix <- function(x = matrix()) {
          Inv <- NULL
          set <- function(y) {
                  x <<- y
                  Inv <<- NULL
          }
          get <- function() x
          setInverse <- function(solve) Inv <<- solve
          getInverse <- function() Inv
          list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}  


## The cacheSolve function checks if the inverse of the matrix has already been 
## saved in the cache and if yes, it returns the saved value. If the cache is empty, 
## the function calculates the inverse of the matrix using the solve function and returns it.

cacheSolve <- function(x, ...) {
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    data <- x$get()
    Inv <- solve(data)
    x$setInvers(Inv)
    Inv
}        


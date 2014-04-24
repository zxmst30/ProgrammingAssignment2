## The functions below can be used to calculate the inverse of a matrix
## and cache the result so that it can be retrieved later to save computing time.


## The makeCacheMatrix funtion creates a special kind of matrix and it can cache its inverse

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
        Inv <- x$getInverse()  #loads the cache from makeCacheMatrix into an object called Inv
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)    #if cache is not empty, return the saved values
        }
        data <- x$get()        # loads the matrix into an object called data
        Inv <- solve(data)     # calculates the inverse of the matrix using the solve function
        x$setInvers(Inv)       # saves the calculated inverse matrix to the cache
        Inv                    # returns the inverse matrix
}


# Sample output from these functions:
> M1 <- makeCacheMatrix(matrix(1:4,2))
> cacheSolve(M1)    # inverse matrix is calculated
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(M1) 
getting cached data # when solving the same matrix again, result is retrieved from cache
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> M2 <- makeCacheMatrix(matrix(5:8,2))
> cacheSolve(M2)    # the inverse of a new matrix is calculated
[,1] [,2]
[1,]   -4  3.5
[2,]    3 -2.5
> 

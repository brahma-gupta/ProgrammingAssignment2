## A pair of functions to calculate and cache the inverse of 
## any given non singular square matrix

## Function to create a special list object to cache the 
## given "matrix" and its inverse. 

makeCacheMatrix <- function(x = matrix()) {

        ## 'x' is a square non singular matrix of any size
        ## 'i' is a variable to cache the inverse matrix
  
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
  
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Function to compute the inverse of non singular square matrix. 
## If the inverse has already been calculated (and the matrix
## has not changed), then this function retrieves from the cache.

cacheSolve <- function(x, ...) {

        ## 'x' is a special matrix created by function 'makeCacheMatrix' 
        ## ... are for named parameters for the 'solve' function

        ## 'data' is a variable to hold the cached matrix object

        ## 'i' is a matrix variable that holds the inverse
        ## to be calculated
        		
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

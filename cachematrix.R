## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This function creates a cache of inverse of the input matrix x
## as per the example of the assignment

makeCacheMatrix <- function(x = matrix()) {
    ## inv is the inverse of the matrix x
    ## Initially it is set to NULL
    inv <- NULL
    
    ## This function sets matrix x as matrix y keeping inv NULL 
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## This function returns the matrix x
    get <- function() {
        x
    }
    
    ## This function sets the inverse of matrix x (inv) as Inverse
    setInverse <- function(Inverse) {
        inv <<- Inverse
    }
    
    ## This function returns the inverse of the matrix x
    getInverse <- function() {
        inv
    }
    
    ## Returns the cache list object with all the functions
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## This function returns the inverse of the matrix x if 
## it is cached or computes it and caches it for future use
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    
    ## Fetches the cache of inverse of the matrix x
    inv <- x$getInverse()
    if(!is.null(inv)){
        ## Checks if inv is not null. If its not null, it returns the cached value
        ## else computes it
        message("getting cached data...")
        return(inv)
    }
    
    ## Get matrix x
    data <- x$get()
    ## Computes inverse of matrix x
    inv <- solve(data, ...)
    ## Sets the inverse
    x$setInverse(inv)
    ## Returns the inverse
    inv
}

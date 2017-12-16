## The below functions are solutions to Programming 
## Assignment 2 in "R Programming" course in Coursera
## The structure of the code is heavily borrowed from the 
## example provided in the assignment which described the
## method to cache the mean of a vector using two functions
## makeVector() and cachemean().

## This function creates a special "matrix" object that 
## can cache its inverse. The "matrix" object is a list of
## functions to set the matrix, get the matrix, 
## set the inverse and get the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv_val) i <<- inv_val
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

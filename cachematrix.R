## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
##rather than compute it repeatedly.
## There are two  functions that cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to:

## 1-set the value of the vector
## 2-get the value of the vector
##set the value of the inverse
##get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinvese = setinverse,
         getinverse = getinverse)
}


##The following function calculates the mean of the special "vector" created with the above function.
##However, it first checks to see if the mean has already been calculated.
##If so, it gets the mean from the cache and skips the computation. 
##Otherwise, it calculates the mean of the data and sets the value of the mean in the cache
##via the setmean function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- inverse(data, ...)
    x$setinv(inv)
    inv
    
}
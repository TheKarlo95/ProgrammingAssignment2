# makeCacheMatrix is a function that returns a list of functions, stores a 
# martix and a cached value of its inverse.
# List contains functions:
#    - setMatrix      set the value of a matrix
#    - getMatrix      get the value of a matrix
#    - cacheInverse   set the cached(inverse) value
#    - getInverse     get the cached(inverse) value

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    set <- function(newValue) {
        x <<- newValue
        cache <<- NULL
    }
    
    get <- function() {
        return(x)
    }
    
    setInverse <- function(inverseMatrix) {
        cache <<- inverseMatrix
    }
    
    getInverse <- function() {
        return(cache)
    }
    
    return(list(set = set
         , get = get
         , setInverse = setInverse
         , getInverse = getInverse))
}


# cacheMatrix is a function that calculates the inverse of a matrix, stores it
# in the cache and returns it.

cacheSolve <- function(x, ...) {
    inverseMatrix <- x$getInverse()

    if(!is.null(inverseMatrix)){
        message("getting cached data")
        return(inverseMatrix)
    }

    inverseMatrix <- solve(x$get())
    x$setInverse(inverseMatrix)

    return(inverseMatrix)
}
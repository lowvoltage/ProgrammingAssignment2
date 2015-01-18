## Below are two functions that are used to create a special object that 
## stores a matrix and cache's its inverse.

## Wraps a matrix into an object that has setters and getter for both the
## original matrix and for its cached inverse matrix

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
         setinverse = setinverse,
         getinverse = getinverse)
}


## Calculates and caches the inverse of a matrix, wrapped in a 
## makeCacheMatrix object. Subsequent calls will reuse the cached result

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## First check for a cached results
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    ## Calculate, cache and return the inverse matrix
    message("computing inverse matrix")
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    inv
}

## The following functions create a wrapper around a matrix that can cache
## its inverse after the first computation.

## Create a wrapper around the given matrix with attached storage for
## caching the inverse of the matrix.  A list of four functions is returned:
## - get() : returns the wrapped matrix value
## - set(x) : sets the wrapped matrix value
## - getinverse() : returns the cached inverse value, or NULL if none is
##   cached
## - setinverse(inverse) : sets the cached inverse value

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set,
         get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}


## Compute the inverse of a matrix wrapped by makeCacheMatrix, returning the
## cached value if possible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (is.null(inv)) {
        message('computing')
        inv <- solve(x$get())
        x$setinverse(inv)
    }
    inv
}

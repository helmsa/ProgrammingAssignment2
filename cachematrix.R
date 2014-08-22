## The function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.
## The function cacheSolve computes the inverse of the special "matrix"
## returned by the makeCacheMatrix function.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## calling $set will set the values of the matrix
## calling $get will return the values of the matrix
## calling $setinverse will save the inverse values of the matrix
## calling $getinverse will return the inverse values of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve takes a "matrix", returned by the makeCacheMatrix function, as
## its argement and computes the inverse of this special "matrix".
## If the inverse was previously computed for the "matrix" it will use the
## cached inverse instead of recomputing it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}

## These function will create a 'special' object that can cache the inverse of a matrix.
## Usage:
## t <- makeCacheMatrix(c)
## x <- cacheSolve(t) - for retrieving the object
##
## t$set(c2) - for changing the matrix to be inverted

## Create a 'special' object that allows caching the inverse of the given matrix.
## The member function set() sets the matrix to be inverted.
## The member function get() returns the matrix to be inverted
## The member function setInverse() updates the cache with a new inverted matrix
## The member function getInverse() returns the cached inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) m <<- inv
    getInverse <- function() m
    list(set = set, get = get,
         getInverse = getInverse,
         setInverse = setInverse)

}


## Returns the inverted matrix of a matrix stored in a 'special' object.
## If the given matrix has not changed, the cached inverted matrix is returned.
## Otherwise, the inverted matrix is computed and the cache of the special object is updated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        m
    }
    matrix <- x$get()
    # get the inverted matrix
    m <- solve(matrix)
    x$setInverse(m)
    m
}

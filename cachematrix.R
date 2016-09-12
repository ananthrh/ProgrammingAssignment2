## The function makeCacheMatrix creates a special object which has functions to get/set the matrix and get/set
## inverse of the matrix
## The function cacheSolve function returns the inverse of a cacheMatrix object

## This function creates a cacheMatrix object with get function to retrieve the matrix, set function to set the matrix
## setinverse to get the inverse of matrix and getinverse to get the inverse of the matrix

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


## This function returns the inverse of the cacheMatrix object. If the inverse of the matrix in cacheMatrix
## has already been calculated, return the cached information.
## If the inverse of the matrix in cacheMatrix has not been calculated, set the inverse and return the 
## newly calculated inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m))
    {
        message('getting cached data')
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}


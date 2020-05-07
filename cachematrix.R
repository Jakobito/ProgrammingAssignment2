## Functions that cache the inverse of a matrix

## makeCacheMatrix creates a special "matrix" object that cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x<<- y
                inverse <<- NULL
        }
        # Get the original matrix
        get <- function() x
        # Set inverse value
        set_inverse <- function(solve) inverse <<- solve()
        # Get inverse value
        get_inverse <- function() inverse
        # Returns the 'special matrix' list 
        list(set = set, 
             get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$get_inverse()
        ## Check if inverse matrix is cached
        if(!is.null(inverse)) {
                message("Getting cached data")
                return(inverse)
        }
        ## If not, calculates the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$set_inverse(inverse)
        inverse
}

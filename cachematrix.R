## functions that compute the inverse of a matrix

## creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL

        ## matrix
        setx <- function( matrix ) {
                x <<- matrix
                inv <<- NULL
        }

        getx <- function() {
                x
        }

        setInverse <- function(inverse) {
                inv <<- inverse
        }

        getInverse <- function() {
                inv
        }

        list(setx = setx, getx = getx,
                setInverse = setInverse,
                getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()

         ## return the inverse if it's set
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }

        ## Get matrix 
        data <- x$getx()

        ## Calculate inverse
        m <- solve(data) %*% data

        ## Set the inverse to the object
        x$setInverse(m)

        ## Return the matrix
        m
}

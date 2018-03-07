## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
        inv <- x$getInverse()
        ## if the inverse has already been calculated        
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## else, here we calculate the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        ## we set the value of the inverse in the cache
        x$setInverse(inv)
        inv
}

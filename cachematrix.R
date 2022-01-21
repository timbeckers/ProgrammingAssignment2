#'The goal of this simple project is to caching the inverse of a matrix
#

## This first  function creates a special "matrix" object that can cache its inverse.
##ARGUMENTS:
#1) x which is a matrix

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinvert <- function(invert) {
                i <<- invert
        }
        
        getinvert <- function() {
                i
        }
        
        
        list(set = set, 
             get = get,
             setinvert = setinvert,
             getinvert = getinvert)
}


# This second function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then cacheSolve should retrieve the inverse 
#from the cache.
#ARGUMENTS:
#1) x which is a special matrix

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinvert()
        
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        mat <- x$get()
        
        i <- solve(mat, ...)
        
        x$setinvert(i)
        
        i
}

###############################################################################
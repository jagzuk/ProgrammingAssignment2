## These funcations cache a matrix and solve its inverse

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        # set/get matrix
        set <- function(y) {
                x <<- y
                m <<- NULL                
        }
        get <- function() x

        # set/get inverse
        setinv <- function(inverse) m <<- inverse
        getinv <- function() m
                
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        
        # check for a cached result
        if(!is.null(m)) {
                message("getting cached data")       
                return(m)
        }

        # otherwise compute inverse
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

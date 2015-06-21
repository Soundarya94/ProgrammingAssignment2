## The R functions given cache potentially time consuming functions – 
## calculating the inverse of the matrix. If the contents of the inverse 
## matrix do not change then we cache the inverse instead of having to 
## look it up again 


## This function creates a special matrix object that can cache the 
## inverse

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

## This function computes the inverse of the special "matrix" returned by 
## the function above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        invmatrix <- x$get()
        m <- solve(invmatrix, ...)
        x$setinverse(m)
        m
}



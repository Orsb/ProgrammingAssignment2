## This function creates a matrix that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the matrix from above. If the inverse has already been calculated and it is the same, then it will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) { # if inverse is cached returns message
                message("getting cached matrix")
                return(i)
        }
        data <- x$get() # if inverse is not found in cache, creates inverse
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
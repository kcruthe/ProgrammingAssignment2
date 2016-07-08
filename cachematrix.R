# Function makeCacheMatrix: This function creates a special "matrix" object that
# can cache its inverse.
#       1.set the value of the matrix
#       2.get the value of the matrix
#       3.set the value of the inverse
#       4.get the value of the inverse


makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}

# Function 2.cacheSolve: This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then the cachesolve should
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
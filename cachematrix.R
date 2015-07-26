# The following functions calculate the inverse of a matrix and writes it to 
# a cache variable such that the next time the previously saved value is 
# returned.

# The makeCacheMatrix function creates a special "matrix",
# which is a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse of the matrix
# 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {       
    # Defines cache.
    cache <- NULL
    set <- function(y) {
        x <<- y
        # Since the matrix is assigned a new value, the cache must be flushed.
        cache <<- NULL
    }
    # Returns matrix x.
    get <- function() {
        x
    }
    # Sets cache to the inverse of matrix x.
    setinverse <- function(inverse) {
        cache <<- inverse
    }
    # Returns the cached inverse of x.
    getinverse <- function() {
        cache
    }
    # Creates a list. Each element of the list is a getter or setter function.
    list(set = set, 
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}

# The following function computes the inverse of a "special" matrix created 
# with makeCacheMatrix. It checks if the inverse has already been computed. 
# In this case, it returns the inverse from cache. Otherwise, the inverse 
# matrix is computed and its value is stored in the cache.
cacheSolve <- function(x, ...) {
    # Gets cached value.
    m <- x$getinverse()
    # If a cached value exists it is returned.
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # Gets matrix, computes the inverse and stores it in the cache.
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}

## Cache time-consuming computations. Matrix inversion is costly computation wise
## thus instead of calculating matrix inverse everytime, cache the inverse 
## while storing the matrix

## Function to create a matrix object that caches the inverse

makeCacheMatrix <- function(x = matrix()) {
       i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Function to compute the inverse of matrix or to return the cached inverse value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		        i <- x$getInverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setInverse(i)
        i
}

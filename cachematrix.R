#Matrix inversion is usually a costly computation and there may be some benefit to caching 
#the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
#will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

#The first function, makeVector creates a special "vector", which is really a list containing a function to

#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the mean
#4) get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- matrixinverse
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

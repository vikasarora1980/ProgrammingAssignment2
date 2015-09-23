# Matrix inversion is usually a costly computation and there can be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly. 
# This assignment is to write a pair of functions that cache the inverse of a matrix.

# The first function, makeCacheMatrix creates a special "matrix" object that can cache its inverse. 
# It's actually a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL #Initialize inv_x as null 
        set <- function(y) { #Set Value for matrix
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x #Get Value for matrix
        setinv <- function(inverse) inv_x <<- inverse #Set Inverse of matrix
        getinv <- function() inv_x #Get inverse of matrix
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv_x <- x$getinv()  
        if(!is.null(inv_x)) { #Check whether existing value of inverse available in cache and return accordingly
                message("getting cached data") 
                return(inv_x) 
        }
        data <- x$get()  
        inv_x <- solve(data, ...) # Calculate Inverse of matrix from data variable
        x$setinv(inv_x) 
        inv_x 
}

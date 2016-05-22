### This function creates a special "matrix" object that can cache its inverse.
#setInverse cached inverse matrix
makeCacheMatrix <- function(x = matrix(1:5, nrow = 3, ncol = 3)) {
        invrs <- NULL
        set <- function(newMatrix){
                x <<- newMatrix
                invrs <<- NULL
        }
        getMatrix <- function() x
        getInverse <- function() invrs
        setInverse <- function(newInvrs) invrs <<- newInvrs;
        
        list(getMatrix = getMatrix,
             getInverse = getInverse,
             setInverse = setInverse)
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated.
##(and the matrix has not changed), then the cachesolve should retrieve
##the inverse from the cache.
cacheSolve <- function(x, ...) {
        
        invrs <- x$getInverse()
        if(!is.null(invrs)){
                message("getting cached data");
                return(invrs);
        }
        
        invrs <- solve(x$getMatrix());
        x$setInverse(invrs);        
        invrs;
}
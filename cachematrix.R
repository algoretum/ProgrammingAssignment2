
## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Creates a special "matrix" object that can store a matrix object 
## and also store/cache the inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x  ## return original matrix
        setinverse <- function( inv ) i <<- inv ## set internal inverse
        getinverse <- function() i ## return inverse
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if( !is.null(i) ) {
                message( "Getting cached inverse")
                return(i)
        }
        data <- x$get()
        i <- solve( data )
        x$setinverse( i )
        i
}

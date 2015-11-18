## makeCacheMatrix contains a list of functions 
## and cacheSolve uses them to return the cached inverse of a matrix
## of calculate it and cache it if not already cached.

## makeCacheMatrix contains functions to set or return the matrix
## and its inverse as required

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cashSolve tests to see if the inverse i already exists
## and if so it returns that cached value.
## and if not it calculates the inverse and then sets the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
        
}

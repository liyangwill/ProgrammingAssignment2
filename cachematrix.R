## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        r <- NULL # r stores the inverse of matrix x
        set <- function (y){
                x <<- y
                r <<- NULL
        }
        get <- function() x
        setinverse <- function(reverse) r <<- reverse
        getinverse <- function() r
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse =  getinverse)

}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        r <- x$getinverse()
        if(!is.null(r)) {
                message("getting cached data")
                return(r)
        }
        data <- x$get()
        r <- solve(data, ...)
        x$setinverse(r)
        r
}

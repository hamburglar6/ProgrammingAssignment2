## A function that creates a list of functions which can be used to set or get 
## a matrix, and then set or get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    m < NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv = function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## A function that gets the cached inverse of matrix x if it has already been computed
## or computes the inverse and caches it if it has not

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if (!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmean(m)
        m
}

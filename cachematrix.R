
## The file contains two functions that can be used to calculate the inverse
## of a matrix. Although the inverse of matrix x can be calculated using solve(x),
## these functions allow the inverse value to be cached, rather than recalcuated
## every time.

## The use case for these functions is for tight loops, where the value of inverse
## is calculated many times, although matrix x may not change. Return of the cached 
## value, instead of repeated calculation of the inverse, will save computation time. 
## If not using tight loops, then solve(x) is more appropriate.


makeCacheMatrix <- function(x = matrix()) {
    ## From the matrix x, creates and returns a list that can be used in conjunction 
    ## with function cacheSolve to find the inverse of x.
    i <- NULL
    set <- function(y) {
        x <<-  y
        i <<- NULL
    }
    get    <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


cacheSolve <- function(x, ...) {
    ## Returns a matrix that is the inverse of 'x'. If the inverse has already been 
    ## calculated, then returns the cached value. Otherwise, calculates the inverse 
    ## using the solve function
    i <- x$getinv()
    if(!is.null(i)) {
        print("Getting cached data")
        return(i)
    }
    data <- x$get()
    i    <- solve(data,...)
    x$setinv(i)
    i
}

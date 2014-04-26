## The two functions below are used to set up a matrix and set/get its
## inverse. If the inverse is already calculated, the second function,
## which is suposed to calculate the inverse, gets the already calcu-
## lated inverse, which is cached on memory.
##
## These functions assume that the matrix is already square and inver-
## tible, that is, nonsingular. There are no tests to certify for it.

makeCacheMatrix <- function(x = matrix()) {
    
    ## This function creates a special "matrix", which is really a list
    ## containing a function to:
    ## 1. set the value of the matrix - makeCacheMatrix$set()
    ## 2. get the value of the matrix - makeCacheMatrix$get()
    ## 3. set the value of the inverse - makeCacheMatrix$setinv()
    ## 4. get the value of the inverse - makeCacheMatrix$setinv()
    
    m <- NULL
    set <- function(y) {
        x <<- y     # x is defined outside; is the argument to the main function
        m <<- NULL  # m is defined outside
    }
    get <- function() x
    setinv <- function(inv) m <<- inv  # m is defined outside this function
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


cacheSolve <- function(x, ...) {
    
    ## This function calculates the inverse of the special "matrix"
    ## created with makeCacheMatrix(). However, it first checks to see
    ## if the inverse has already been calculated. If so, it gets the
    ## inverse from the cache and skips the computation. Otherwise, it
    ## calculates the inverse of the data and sets the value of the in-
    ## verse in the cache via the setinv function.
    ## 
    ## The matrix is assumed to be square and nonsingular.
    
    m <- x$getinv()
    if(!is.null(m)) {  # gets the cached inverse; doesn't need to calculate
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)  # calculates the inverse
    x$setinv(m)
    m  # returns the calculated inverse
}

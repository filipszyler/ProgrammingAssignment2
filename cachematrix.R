## The below functions use R scoping rules to cache
## the inverse of the given matrix (computed by solve() function)
## in order to prevent repeating of time-consuming computations
##
##
## Sample usage:
##
## > sample <- matrix(rnorm(25), nrow = 5)
## > csample <- makeCacheMatrix(sample)
## > cacheSolve(csample) ## inverse computed
## > cacheSolve(csample) ## inverse retrieved from cache


## The makeCacheMatrix() function creates a special
## "matrix" which is in fact a list containing a functions to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    
    ## s stores cached inverse of the matrix
    s <- NULL
    
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## get the value of the matrix
    get <- function() x
    
    ## set the value of the inverse of the matrix
    setsolve <- function(solve) s <<- solve
    
    ## get the value of the inverse of the matrix
    getsolve <- function() s
    
    ## return "matrix" (list) of functions defined within
    ## makeCacheMatrix() function
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}



## The cacheSolve() function either compute the inverse of
## the matrix created with the function makeCacheMatrix ()
## or retrieves it from the cache if it has been already
## computed
cacheSolve <- function(x, ...) {
    ## get cached inverse (it can be NULL)
    s <- x$getsolve()
    
    ## checks if the inverse of the given matrix has been already
    ## calculated (is not NULL); if true then returns it from
    ## the cache, skipping the computation
    if(!is.null(s)) {
        message("getting cached solve data")
        
        ## return cached inverse
        return(s)
    }
    
    ## if the cached inverse of the given matrix is not present:
    ## get the matrix
    data <- x$get()
    
    ## compute the inverse of the matrix
    s <- solve(data, ...)
    
    ## cache the computed inverse
    x$setsolve(s)
    
    ## return the computed inverse
    s
}

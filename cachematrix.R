## This pair of functions enables caching and subsequent retrieval of a matrix's inverse value, in order to speed processing of
## large datasets. The first function, makeCacheMatrix(), establishes four functions that get and set (retrieve and modify) data values
## for the matrix x and its inverse i. This function also creates a list object of type makeCacheMatrix() in the parent environment that
## stores a matrix and its inverse. The second function, cacheSolve(), uses the list object created by makeCacheMatrix() to retrieve the cached
## matrix inverse or populate a new inverse.

## This function establishes four functions that get and set (retrieve and modify) data values for the matrix x and its inverse i.
## This function also creates an object of type makeCacheMatrix() in the parent environment that stores a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                       ## Initialize i with value NULL
        set <- function(y) {        
                x <<- y                                 ## Set argument y to the x object in the parent environment
                i <<- NULL                              ## Clear any value of i set by previous use of cacheSolve
        }
        get <- function() x                             ## Define getter for function x
        setinv <- function(solve) i <<- solve           ## Set solve function and set value of solve to i in parent environment.
        getinv <- function() i                          ## Get value of i from parent environment.
        list(set = set, get = get,                      ## Create new object of type makeCacheMatrix() to be used in cacheSolve()
             setinv = setinv,
             getinv = getinv)
}


## This function uses the list object created by makeCacheMatrix() to retrieve the cached matrix inverse or populate a new inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()                                 ## Attempt to retrieve matrix inverse from makeCacheMatrix
        if(!is.null(i)) {                               ## If a value exists for i (i.e. inverse of matrix has been taken previously),
                message("getting cached data")          ## return stored value of i to parent environment
                return(i)
        }
        data <- x$get()                                 ## If no stored inverse for matrix, get value of x
        i <- solve(data, ...)                           ## Find inverse of matrix x
        x$setinv(i)                                     ## set the inverse in the input object
        i                                               ## print matrix inverse i
}

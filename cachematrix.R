makeCacheMatrix <- function(x = matrix()) {
        
        # declare a variable to inverse a matrix
        inv <- NULL
        
        # set the initial value of the matrix
        set <- function(iMatrix) {
                # set the global variable x to the initial value of the matrix
                x <<- iMatrix
                
                # set the varible used to inverse the matrix to 
                # a null value
                inv <<- NULL
        }
        
        # retrieve the initial matrix
        get <- function() x
        
        
        # set an inverse to the initial matrix
        setInv <- function(inv) cachedInverse <<- inv
        getInv <- function() inv
        list(
             set = set, get = get,
             setInv = setInv,
             getInv = getInv
             )
}

## this function pulls variables from memory if the the data is cached

cacheSolve <- function(x, ...) {
        invF <- x$getInv()
        ## Checks to see if the matrix has been defined (if it is not a null value)
        if(!is.null(invF)) {
                message("getting cached data")
                return(invF)
        }
        data <- x$get()
        invF <- solve(data)
        x$setInv(invF)
        invF
}


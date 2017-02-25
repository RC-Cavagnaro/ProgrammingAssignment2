## Objective: create a matrix object and provide a mechanism to cache the 
## inverse of that matrix in the event the matrix has not changed

## Constructor function for the matrix object
## Makes a list of four functions or values
## Set matrix: initalizes the matrix
## Get matirx: returns the matrix
## Set Inverse of Matrix: moves to inverse function and solves for the inverse.
## Get Inverse of Matrix: returns inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    ## Set inverse solved back to null on first run
    mInv <- NULL
    
    ##----Functions----##
    ##Set Initalizes Matrix and sets cached inverse to NULL
    set <- function(y) {
        x <<- y
        mInv <<- NULL
    }
    
    ## returns matrix
    get <- function() x
    
    ## set inverse function sets value of m
    setInverse <- function(inverse) mInv <<- inverse
    
    ## returns inverse matirx held in m
    getInverse <- function() mInv
    
    ##----List of functons that make up this class as a vector
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## Get value of M from the MatrixList above
    inv <- x$getInverse()
    
    ## Check if inv is null if not use that value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## We end up here if M was null so we will compute the inverse 
    ## of the input matrix
    ## Get data from original matrix and set inv to the result of the solve 
    ## function 
    inv <- solve(x$get(), ...)
    
    ## set inv in the class above so we can use cached data
    x$setInverse(inv)
    
    ## return inverse matrix
    inv
}


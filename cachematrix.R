## Put comments here that give an overall description of what your
## functions do:

## makeCacheMatrix is used to caching matrices and their inverses.
## It allows users to store, retrieve, and update a matrix.
## The global variables m and invMatrix serve as the cache for the matrix and its inverse.

makeCacheMatrix <- function(m = matrix()) {
    
    invMatrix <- NULL   ## Initialize invMatrix with a value of NULL

    ## setMatrix assigns a new matrix to the global variable x
    ## By calling set(new_matrix), the user can update the cached matrix.
    setMatrix <- function(new) {  
        m <<- new
        invMatrix <<- NULL
    }
    ## Cached matrix stored in global variable x is returned to user
    getMatrix <- function() m
    
    ## The setInvMatrix function ensures that the cached inverse matrix is updated
    ## when the user provides a new inverse matrix.
    setInvMatrix <- function(inv) {
        invMatrix <<- inv
    }
    
    ## returns the cached inverse matrix stored in the global variable invMatrix
    getInvMatrix <- function() invMatrix
    
    ## makeCacheMatrix function returns a list containing the implemented functions:
    list(setMatrix = setMatrix, getMatrix = getMatrix, 
         setInvMatrix = setInvMatrix, getInvMatrix = getInvMatrix)
}


## cacheSolve makes the computation of matrix inverses faster by reusing
## previously calculated results when available.

cacheSolve <- function(m, ...) {
    
    ## checks if the inverse matrix is already cached by calling m$getInvMatrix()
    invMatrix <- m$getInvMatrix()
    
    ## If so, it prints a message indicating that the cached matrix is being used
    ## and returns the cached value.
    if (!is.null(invMatrix)) {
        message("This is the cached matrix")
        return(invMatrix)
    }
    ## If the matrix is not cached, it prints a message and retrieves the
    ## original matrix using m$getMatrix().
    message("This is the matrix")
    data <- m$getMatrix()
    invMatrix <- solve(data, ...)
    m$setInvMatrix(invMatrix)
    invMatrix
}

# input to function makeCacheMatrix: square matrix object
# output of function makeCacheMatrix: a list of 4 functions that 
#     1. sets the value of the inverse cache: NULL represents a new calculation is required
#     2. gets the falue of the inverse cache
#     3. sets the value of the matrix to be inverted
#     4. gets the value of the matrix to be inverted

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
     
       # set the inverse cache to NULL to indicate a new matrix must be inverted
      inverseCache <- NULL
      
      # this function enables a new matrix to be entered
      # input to function setMatrix is: square matrix object
      setMatrix <- function(newMatrix) {
            # stores a new matrix into x
            x <<- newMatrix
            # sets inverseCache to NULL because there is now a 
            # different matrix to invert
            inverseCache <<- NULL
      }
      
      # function returns the matrix to be inverted
      getMatrix <- function() {x}
      
      # function stores new inverse matrix into the inverse cache
      setInverseCache <- function(newInverse) inverseCache <<- newInverse
      
      # function returns contents of inverse matrix cache
      getInverseCache <- function() {inverseCache}
      
      #return value of function is a list of key functions
      list(setMatrix = setMatrix, 
           getMatrix = getMatrix,
           setInverseCache = setInverseCache,
           getInverseCache = getInverseCache)
}


# input to function cacheSolve: a list of four functions that is an output of the constructor 
# function makeCacheMatrix:
#     x$setMatrix: (optional)
#     x$getMatrix: (required) Returns the value of the matrix so inverse can be calculated   
#     x$setInverseMatrix: (required) To be exclusively used by this function CacheSolve
#                                    to store the calculated inverse matrix into the cache
#     x$getInverseMatrix: (optional)  
# output of function cacheSolve: an inverted matrix or error message
cacheSolve <- function(x, ...) {

      # Loads the value of the inverse cache to determine whether to calculate inverse
      inverseCache <- x$getInverseCache()
      
      # if inverse cache is NOT NULL, pull the precalculated inverse from the cache
      if(!is.null(x$getInverseCache())) {
            message("getting cached data")
            return(inverseCache)
      }
      
      # else, the inverse cache is NULL, so calculate inverse matrix
      inverseCache <- solve(x$getMatrix())
      
      # Assign a nonNULL value to inverseCache so the function knows 
      # to not recalculate the inverse until a new matrix is entered
      x$setInverseCache(inverseCache)
      
      # return inverse matrix value
      inverseCache
}

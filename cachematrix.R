## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##############################################################################
## Description: makeCacheMatrix creates a special "matrix" object that chaches
## its inverse for improving matrix inverse calculation performance.
##
## Usage:
## makeCacheMatrix(matrix(c(1, 1, 2, 2, 3, 2, 2, 1, 1), nrow=3, ncol=3))
## makeCacheMatrix(matrix(rnorm(10000), ncol=100, nrow=100))
##
## Arguments: 'x' is an R matrix
##
## Value: makeCacheMatrix returns really a list of four functions:
## 1- set: to set the value of the matrix 'x'
## 2- get: to get the value of the matrix 'x'
## 3- setinverse: to set the value of the inverse of the matrix 'x'
## 4- getinverse: to get the value of the inverse of the matrix 'x'
##
## Note: The argument 'x' is assumed to be a square invertible matrix.
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
   ## "inv" store the inverse of the matrix 'x' in parent environment
   ## assign NULL to "inv" to indicate that it is not calculated yet
   inv <- NULL   
   
   ## The function set  takes a new matrix "y" and assign it to "x"
   ## and set "inv" to NULL to indicate that x has been changed
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   
   ## get return the actual matrix "x"
   get <- function() x
   
   ## setinverse takes "newinv" the new inverse of "x" calculated by the 
   ## function cacheSolve and assign it to the stored inverse "inv"
   setinverse <- function(newinv) inv <<- newinv
   
   ## getinverse returns "inv" the store inverse of "x"
   getinverse <- function() inv
   
   ## return the list of four functions set, get, setinverse and getinverse
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
   
}


## Write a short comment describing this function
##############################################################################
## Description: cacheSolve produces the inverse of its argument 'x'
##
## Arguments:
## x : is a special "matrix" object created with the function makeCacheMatrix,
##     it is a list of functions which encapsulates a squared invertible matrix
##
## Value: returns the inverse of its argument 'x'
##
## Usage:
## m <- makeCacheMatrix(matrix(c(0, 1, 1, 0), nrow=2, ncol=2))
## cacheSolve(m)
##############################################################################

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   
   ## get the value of stored inverse of 'x' 
   inv <- x$getinverse()

   ## test if the stored inverse is ready ('inv' is not NULL), so return it
   if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
   }
   
   ## As the inverse is not calculated yet, invoke 'x$get' to get the matrix 
   ## value and then calculate its inverse by using the function 'solve'
   data <- x$get()
   inv<- solve(data, ...)
   
   ## save the new calculated inverse by invoking 'x$setinverse' function
   x$setinverse(inv)
   
   ## return the new calculated inverse of 'x'
   inv
}

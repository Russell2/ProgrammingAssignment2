##################
## cachematrix.R
## 
## This is a pair functions that cache the inverse of a matrix.
##################

##################
## The function 'makeCacheMatrix' creates a special R object that is four
## functions containing special pair of "matrices" 'x' and its inverse 'ix' 
## that are both cached in the object. And a list which associates the lables
## and functions.
##
## The function takes one argument:
##  x ----- a square numeric or complex matrix containing the coefficients of
##          the linear system. Logical matrices are coerced to numeric. Using
##          'solve' to find the inverse. It is also ussumed the matrix is 
##          invertible.
##
## Returns an a function containing the invertible matrix 'x' and its inverse
## 'ix'
##
###################
makeCacheMatrix <- function(x = matrix()) {
        ## set the inverse matrix initially to NULL
        ix <- NULL
 
        ## set the value of the matrix
        set <- function(y) {
                x <<- y
                ix <<- NULL
        }
        ## get the value of the matrix
        get <- function() x

        ## set the value of the inverse matrix
        setinvx <- function(solve) ix <<- solve

        ## get the value of the inverse matrix
        getinvx <- function() ix

        ## 
       list(set = set, get = get,
             setinvx = setinvx,
             getinvx = getinvx)
}

##################
## This function computes the inverse of the special "matrix" function returned
## by 'makeCacheMatrix' above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve retrieves the inverse from the 
## cache.
##
## The function takes multiple arguments:
##  x ----- the special "matrix" function that contains the matrix that is 
##          assumed to be invertible
##  ... --- further arguments passed to or from 'solve' or other methods
##
## Returns a matrix that is the inverse of the matrix imbedded in x  
##
###################

cacheSolve <- function(x, ...) {
        ## get the value of the inverse matrix
        ix <- x$getinvx()
        ## if it is cached return that
        if(!is.null(ix)) {
                message("getting cached data")
                return(ix)
        }
        ## if not cached yet, find the inverse, cache it, and return it
        data <- x$get()
        ix <- solve(data, ...)
        x$setinvx(ix)
        ix
}


## This file contains two main functions that, when used together, compute and return  
## the inverse of a matrix if it has not been previously computed, or it returns
## the cached value of the matrix inverse when available.  The idea is to save
## potentially long computations by returning a cached inverse value for the matrix 
## inverse.
##
## Use example:  vx1 <- makeCacheMatrix(x3)
##                cacheSolve(vx1)
##
##   where x3 is an nxn (square) invertible numeric matrix
##   The first call to cacheSolve(vx1) will compute x3-inverse, subsequent
##   calls will return the cached value provided the matrix values are unchanged.
##   If the matrix stored in vx1 is changed, a new inverse will be computed 
##   when cacheSolve is called again.  
##
##
## The main first function, makeCacheMatrix, creates a special "matrix" object
## which encapsulates a set of data structures and methods (i.e. nested functions)
## that operate on the data structures.  The previous values are stored in 
## this "matrix object", and their values are set and retrieved though the 
## (nested) functions when not inside the object."  Multiple levels of scoping 
## apply; one inside the object (via the <<- operator) and another from outside
## the object (via remote calls such as vx$get(), where vx is a "special matrix").

## The three data structures inside a "special matrix" are:
##  1.  a numeric square matrix x, whose inverse value is to be cached.
##      We are told to ssume x is non-singular (so the inverse exists)
##  2.  a numeric square matrix xp, the previous value of x
##       In the assignment there is a parenthetical comment that the
##       cached inverse is returned if it exists AND the value of the matrix
##       has not changed.  xp is used to test whether x and xp are identical.  
##       xp is first set when the inverse of x is computed.
##  3.   a numeric square matrix xi, the inverse of x is stored once computed.

## There are six methods (or function calls), only one of which is not called 
## by cacheSolve, but is included for the sake of completeness, noting it in 
## the description.

##  1. set - (re)sets the value of matrix x after setting the value of xp to x
##     UNUSED by cacheSolve but allows a mechanism to change x external to
##     the object containing x, xp, and xi.
##  2. get - returns the value of matrix x
##  3. setxp - (re)sets the value of matrix xp to the previous, and possibly
##     current value of x.
##  4. getxp - returns the value of matrix xp (possibly NULL)
##     used in cacheSolve to test if x is unchanged by comparing it to xp.
##  5. setinv - sets xi to the inverse of x
##  6. getinv - returns the value of xi (possibly NULL)


makeCacheMatrix <- function(x = matrix()) {
  
    xp <- NULL   # matrix values set from last call 
    xi <- NULL   # inverse of xp if previously set
  
    set <- function(y) {     
        xp <<- x
        x <<- y
        xi <<- NULL   
    }
  
    get <- function() x
    setxp <- function(cur) xp <<- cur
    getxp <- function() xp
    setinv <- function(inv) xi <<- inv
    getinv <- function() xi
    
    list(get = get,  set = set, 
         setxp = setxp, getxp = getxp,
          setinv = setinv, getinv = getinv)  

}


## The second main function, cacheSolve, computes the inverse of the matix 
## defined by the object returned by makeCacheMatrix.   If the inverse exists 
## AND the matrix has not changed, cacheSolve returns the cached inverse otherwise
## it returns a newly computed inverse using the built-in R function 
## solve().  
## A message is output indicating whether a valid cached inverse was available
## or if a it was necessary to compute the inverse.  The inverse matrix is
## returned and also displayed.  

cacheSolve <- function(x, ...) { 
  
    xi <- x$getinv()
    xp <- x$getxp()
    data <- x$get()
    
    if((identical(data,xp)) & (!is.null(xi))){
        message("returning cached inverse")
        return(xi)
    }
    
    xi <- solve(data, ...)
    x$setxp(data)
    x$setinv(xi)
    message(" returning newly computed inverse")
    xi
} 


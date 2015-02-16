## Put comments here that give an overall description of what your
## functions do

################################################################################
## cacheSolve retrieves the inverse of the matrix if the inverse is already 
## calculated.  If not it calculates the inverse and returns that.  It uses the
## makeCacheMatrix function to store the values.
################################################################################
## makeCacheMatrix creates a list of functions to set and get a matrix and to 
## set and get its inverse.  It does not calculate an inverse; it merely stores
## it and retrieves it when needed.  It also determines if the matrix is changed
## and updates the matrix and nulls the inverse if it is changed
################################################################################
## Write a short comment describing this function
## makeCacheMatrix takes a matrix as an argument.  If none is provided it 
## creates a null matrix.  It returns a list of functions.  The functions are -
################################################################################
##      set()
################################################################################
##      set() is used to update the matrix stored in makeCacheMatrix.  If the 
##      matrix is new then the inverse is set to a new empty matrix
################################################################################
##      get()
################################################################################
##      get() is used to retrieve the matrix stored in makeCacheMatrix
################################################################################
##      setinv()
################################################################################
##      setinv() is used to store the value of the inverse of the matrix.  The
##      inverse is not calcuated in this function
################################################################################
##      getinv()
################################################################################
##      getinv() is used to retrieve the value of the inverse matrix.  If no 
##      inverse has been stored this will be an empty matrix of one NA value
###############################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- matrix() ## initialize a placeholder for the inverse
   
    set <- function(y){
        if(!identical(x,y)){  ## only update if y is a different matrix
            x <<- y     ## set the x to the matrix passed in (y)
            inv <<- matrix()    ## initialize the inverse to an empty matrix
        }
    }
    
    get <- function() x ## return the value of the matrix
    
    setinv <- function(solve)  inv <<- solve  ## set the value of the inverse 
    
    getinv <- function() inv  ## return the value of the cached matrix
     
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
################################################################################
## cacheSolve takes a list of functions created by the makeCacheMatrix function
## it checks to see if the inverse has been calculated and if so returns it.
## If not cacheSolve calculates the inverse, updates the list with the new value
## and returns the inverse.  Options to the solve function can be provided after
## the list created by makeCacheMatrix.  Getting cached data message has been 
## added to simplify testing and review
################################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()  ## check the current value of the inverse
    if(!identical(inv, matrix())){  ## determine if the inverse is empty
        message("getting cached data") ## let the user know the data are cached
        return(inv)
    }
    data <- x$get() ## if the inverse is empty then retrieve the matrix
    inv <- solve(data, ...)  ## calculate the inverse
    x$setinv(inv)  ## update the cache with the inverse
    inv ## return the inverse
}

## END of assignment.  functions below are alternate versions

################################################################################
################################################################################
## makeCacheMatrixAlt is an alternative way to write the function.  It provides
## a boolean to let the cacheSolveAlt know if the inverse is set.
## makeCacheMatrixAlt creates a list of functions to set and get a matrix and to 
## set and get its inverse.  It does not calculate an inverse; it merely stores
## it and retrieves it when needed.  It also determines if the matrix is changed
## and updates the matrix and nulls the inverse if it is changed

## Write a short comment describing this function
## makeCacheMatrix takes a matrix as an argument.  If none is provided it 
## creates a null matrix.  It returns a list of functions.  The functions are -
##      set()
##      set() is used to update the matrix stored in makeCacheMatrix.  If the 
##      matrix is new then the inverse is set to a new empty matrix
##      get()
##      get() is used to retrieve the matrix stored in makeCacheMatrix
##      setinv()
##      setinv() is used to store the value of the inverse of the matrix.  The
##      inverse is not calcuated in this function
##      getinv()
##      getinv() is used to retrieve the value of the inverse matrix.  If no 
##      inverse has been stored this will be an empty matrix of one NA value
##      isset()
##      isset() returns a boolean TRUE if the inverse is set or FALSE if it is 
##      still empty and needs to be set via setinv()
################################################################################
makeCacheMatrixAlt <- function(x = matrix()) {
    
    inv <- matrix() ## initialize a placeholder for the inverse
    invset <- FALSE
    set <- function(y){
        if(!identical(x,y)){  ## only update if y is a different matrix
            x <<- y     ## set the x to the matrix passed in (y)
            inv <<- matrix()    ## initialize the inverse to an empty matrix
            invset <<- FALSE
        }
    }
    
    get <- function() x ## return the value of the matrix
    
    setinv <- function(solve)  {
        inv <<- solve  ## set the value of the inverse 
        invset <<- TRUE
    }
    
    getinv <- function() inv  ## return the value of the cached matrix
    
    isset <- function() invset ## return whether the inverse is set
    
    list (set = set, get = get, setinv = setinv, getinv = getinv, isset = isset)
}


## Write a short comment describing this function
################################################################################
## cacheSolveAlt takes a list of functions created by the makeCacheMatrixAlt 
## function and is an alternate solution that uses the isset() boolean.
## it checks to see if the inverse has been calculated by looking at isset()
## and if so returns it.  If not cacheSolve calculates the inverse, 
## updates the list with the new value  and returns the inverse.  
## Options to the solve function can be provided after the list created by 
## makeCacheMatrixAlt.  The getting cached data message is
## added to simplify review and testing
################################################################################

cacheSolveAlt <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    if(x$isset()){  ## determine if the inverse is set
        message("getting cached data") ## let the user know the data are cached
        return(x$getinv())
    }
    data <- x$get() ## if the inverse is empty then retrieve the matrix
    inv <- solve(data, ...)  ## calculate the inverse
    x$setinv(inv)  ## update the cache with the inverse
    inv ## return the inverse
}

## cacheInverse is a simpler alternate version using a more object oriented 
## style.  It  returns a list with a pseudo-property and one pseudo-method 
## that provides the inverse.
## The property is the matrix it can be obtained through get() and updated 
## with set().  The method is inv() and it will return the inverse if cached 
## and calculated it if not cached.  The Returning from cache message is added
## to simplify review. 

cacheInverse <- function(x = matrix()){
    
    s <- matrix() ## initialize a placeholder for the inverse
    isset <- FALSE
    set <- function(y){
        if(!identical(x,y)){  ## only update if y is a different matrix
            x <<- y     ## set the x to the matrix passed in (y)
            s <<- matrix()    ## initialize the inverse to an empty matrix
            isset <<- FALSE
        }
    }
    
    get <- function() x ## return the value of the matrix
    
    inv <- function(...)  {
        if(isset)
        {
            message("Returning from cache")
            return (s)
        }
        s <<- solve(x, ...)## set the value of the inverse 
        isset <<- TRUE
        s
    }
    
    list (set = set, get = get, inv = inv)
}

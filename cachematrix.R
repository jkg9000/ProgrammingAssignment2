## The purpose of this file is to create and cache the inverse of a matrix,
## in order to reduce the cost of computing the same inverse matrix over and 
## over again


## makeCacheMatrix allows a matrix to be passed in, then stores it in the cache,
## and includes setters and getters for the matrix and its inverse

## call like this:   matrixObj = makeCacheMatrix(yourOriginalMatrix)


makeCacheMatrix <- function(x = matrix()) {
    
    ## clear out matInv, as a new matrix is being passed in
    xInv <- NULL
    
    # set a new matrix, store in cache
    set <- function(y) {
        
        ## matInit is the passed-in matrix that will be stored and referenced
        x <<- y
        
        ## matInv will hold the inverse of matInit
        ## when matInit is replaced (as above), clear out matInv, 
        ## otherwise it will still hold the inverse of the *previous* matrix.
        xInv <<- NULL
    }
    
    ## get() returns the original, non-inverted matrix
    get <- function() x
    
    ## setInv() sets the passed-in matrix as matInv, the inversed matrix
    setInv <- function(inv) xInv <<- inv
    
    ## getInv() returns the inversed matrix, matInv, from the cache
    getInv <- function() xInv
    
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve is expecting the param x, which is the object returned
## by makeCacheMatrix.  

## use like this, where matrixObj was created by running makeCacheMatrix:
## matrixInverse = cacheSolve(matrixObj)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xInv <- x$getInv()
        
        ## if xInv is not null, go get the cached data and return it
        if(!is.null(xInv)) {
            message("getting cached data")
            return(xInv)
        }
        
        ## if xInv is null (there wasn't any cached inverse),
        ## get the matrix, create the inverse, cache it, then return it
        data <- x$get()
        message("setting new inverse")
        xInv <- solve(data)
        message("putting new inverse in cache")
        x$setInv(xInv)
        xInv
}

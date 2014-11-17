## 2 different functions; (1) create matrix and set it to do Inverse 
## of it; (2) cache's the inverse of the any given matrix; so it can
## be recalled later with doing computation if the matrix matches...

##    (1) Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # set the inverted matrix to NULL
    invMatrix <- NULL
    
    # set() - allows a new matrix to be set into the function. 
    #   this call will invalidate the computed cached inverse matrix
    set <- function(y) {
        x <<- y
        invMatrix <<- NULL
    }
    
    # get() - simply returns the original matrix
    get <- function() x
    
    # setinverse() allows an inverted matrix to be set in this function
    setinverse <- function(inverse) invMatrix <<- inverse
    
    # getinverse() simply return the inverted matrix. It would return null, if 
    #   an inverted matrix has not been set
    getinverse <- function() invMatrix
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}

## (2) Compute the inverse of the matrix returned from (1); it can
##retrieve the inverse of the matrix from the cache if the matrix has
##not changed, otherwise it computes inverse and save in cache

cacheSolve <- function(x, ...) {
    
    # get the inverted matrix from the passed in makeCacheMatrix() instance
    invMatrix <- x$getinverse()
    
    # if inverted matrix is not null (i.e. already computed), return 
    #       inverted matrix. Otherwise the code will proceed to compute the inverted matrix
    #       This acts as the cache 
    if (!is.null(invMatrix)){
        message("getting cached data")
        return(invMatrix)
    }
    
    # get the original matrix
    data <- x$get()
    
    # use solve function to compute the inverse
    invMatrix <- solve(data)
    
    # set the inverst back into the makeCacheMatrix() instance
    x$setinverse(invMatrix)
    
    # return the inverted matrix
    invMatrix
}

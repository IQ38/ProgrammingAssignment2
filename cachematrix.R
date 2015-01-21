## makeCacheMatrix is a constructor function that that takes a matrix of numeric values as its input
## its purpose is to store the the inverse of the matrix that is passed into it
## child functions set() and get() allow you to update and retrieve the matrix being used
## child functions setinverse() and getinverse() allow you to set and retrive the inverse of that matrix
## the main function returns a list of objects returned by its child functions

## cacheSolve() retrieves the inverse of 'x' from makeCacheMatrix() if it exists
## otherwise it just calculates the inverse of 'x' and stores it in makeCacheMatrix()


## take a matrix as an input value and allow its inverse to be stored and retrieved for later use
makeCacheMatrix <- function(x = numeric()) {
    i<-NULL  
    
    set <- function(y) {
        ## proceed only if any FALSE matches appear in comparing two matrices
        # which means 'y' is a new matrix
        if (!any(x %in% y)) {
            x <<- y
            i <<- NULL
        }
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve  
    getinverse <- function() i 
    
    list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## retrieve the cached inverse of a matrix if the cache exists otherwise calculate it and store it
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
    	## cached value exists so retrieving it
        message("getting cached data")
        return(i)
    }
    
    ## cached value DOES NOT exist so calculating and storing it
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i    
}

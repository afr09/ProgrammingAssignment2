#Initializing m and inv(m) and return an list of methods 

makeCacheMatrix <- function(x = matrix()) {
        
        # Initialize the inverse m
        i <- NULL
        
        # setting the matrix
        set <- function( matrix ) {
                x <<- matrix
                i <<- NULL
        }
        
        # Getting the matrix
        get <- function() {
                x
        }
        
        ##set the inverse of the matrix
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        #get the inverse of the matrix
        getInverse <- function() {
                i
        }
        # Return a list 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## Solver of the inverse of m

cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        # Return the inverse if its already set
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        # Get the matrix from our object
        data <- x$get()
        # Calculate the inverse using matrix multiplication
        i <- solve(data, ...)
        # Set the inverse to the object
        x$setInverse(i)
        # Return the matrix
        i
}


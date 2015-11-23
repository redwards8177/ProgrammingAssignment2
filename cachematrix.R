makeCacheMatrix <- function(x = matrix()) {
  ## creates a square invertible matrix and
        ## returns a list containing functions to
        ##              1. set the value of the matrix
        ##              2. get the value of the matrix
        ##              3. set the value of the inverse
        ##              4. get the value of the inverse
        ##         this list is used as the input to cacheSolve()

        m <- NULL
        set <- function(y) {
		
		# use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 
                
		x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- mean
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


cacheSolve <- function(x, ...) {
	## uses output of makeCacheMatrix() and
	## calculates the inverse of the original matrix input to makeCacheMatrix()

        m <- x$getinverse()

	## checks to see if the inverse has already been calculated
        
	if(!is.null(m)) {
		## if so, get the inverse from the cache and skip computation. 
                message("getting cached data")
                return(m)
        }

	## otherwise, calculates the inverse of the data 

        data <- x$get()
        m <- inverse(data, ...)

	## sets the value of the inverse in the cache via the setinverse function.
        x$setinverse(m)
        
	return (m)
}

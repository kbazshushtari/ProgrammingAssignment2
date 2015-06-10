
## The function below creates a special "matrix" object that can cache its inverse 
## An assumption made is that the matrix supplied will always be invertible

makeCacheMatrix <- function(x = matrix()) {
	m <-  NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set = set, get = get,
			setInverse = setInverse,
			getInverse = getInverse)
	
}


## The function below will actual compute the inverse of the special "matrix" formed in the function below. 

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
        	message("getting cached data")
        	return(m)
        }
        data = x$get()
        m <- solve(data,...)
        x$setInverse(m)
        m
}

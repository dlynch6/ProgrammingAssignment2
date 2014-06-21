## In this example we use the <<- operator which assigns a value to an object in an environment
## that is different from the current environment.
## Below are two functions that are used to cache an Inverse of a matrix

## This first function, makeCacheMatrix creates a special "matrix", which is really a list containing 
## a function to
##	set the value of the matrix
##	get the value of the matrix
##	set the value of the Inverse
##	get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	get <- function () x
	setInverse <- function(solve) m <<- solve
	getInverse <- function() m
	list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the Inverse of the special matrix created with the above function.
## However, it first checks to see if the Inverse has already been calculated.
## If so, it gets the Inverse from the cache 
## and skips the computation.
## Otherwise, it calculates the Inverse of the data and 
## sets the value of the Inverse in the cache via the setInverse function.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
	cacheMatrix <- makeVector(x)
	m <- x$getInverse()
	if(!is.null(m) | x == x$get()) {
		message("getting cached data")
		return(m)
	}
	matrix <- x$get()
	m <- solve(matrix, ...)
	x$setInverse(m)
	m
}

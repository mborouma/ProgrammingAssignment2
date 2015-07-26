## This program contains a pair of functions, makeCacheMatrix and cacheSolve,
## for computing and caching the inverse of an invertible matrix

## makeCacheMatrix - creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL

	## set the matrix
	set <- function(y) {
		x <<- y
		m <<- NULL
	}

	## get the matrix
	get <- function() x

	## set the inverse of the matrix
	setinverse <- function(solve) m <<- solve

	## get the inverse of the matrix
	getinverse <- function() m

	## list of functions 
	list(set = set, get = get, 
	     setinverse = setinverse,
	     getinverse = getinverse)
}


## cacheSolve - computes inverse of special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}

	## get the special matrix, compute inverse, and cache
	data <- x$get()
	m <- solve(data)
	x$setinverse(m)

        ## Return a matrix that is the inverse of 'x'
	m
}

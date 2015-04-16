## These functions can be used to obtain inverse matrices from any invertible matrix.
## Because matrix inversion can be a costly computation, the result of inversion shall
## be stored in cache, instead of being computed repeatedly.


## With makeCacheMatrix it is possible to set the matrix a user is interested in
## and compute it's inversion, using solve() function. The argument of this function
## has to be an invertible matrix. Result of the inversion is stored in cache (m).

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y 						# Setting the matrix
		m <<- NULL
	}
	get <- function() x 				# Can be used for confirmation
	setinv <- function(val) m <<- val 	# Computing the inversion
	getinv <- function() m 				# Retrieving the result of computation
	list(set = set, get = get,
		 setinv = setinv,
		 getinv = getinv)
}


## cacheSolve() is used to compute the inverse using the matrix returned by makeCacheMatrix()
## function. If the inverse has already been calculated, and the matrix has not been changed,
## it retrieves the result of inversion from cache.

cacheSolve <- function(x, ...) {
	m <- x$getinv()
	if(!is.null(m)) {					# If the computation has already taken place, return data stored in cache
		message("Getting cached inverse matrix...")
		return(m)
	}
	message("Computing matrix inversion...")
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
        ## Return a matrix that is the inverse of 'x'
}

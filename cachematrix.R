## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Cache a matrix and the inverse in the parent environment
## Create a cache in the parent environment to store local matrix and it's inverse
## Use: cMatrix <- makeCacheMatrix(x)
## parameter: x - square matrix
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL						# create a cache store for invest matrix
	set <- function(y) {			# function to set value of cache store matrix
		x <<- y						# set matrix cache to input parameter
		inv <<- NULL				# initialize cache store for inverse
	}
	get <- function() x				# return cached matrix
	setinv <- function(i) inv <<- i # set inverse matrix to input parameter
	getinv <- function() inv		# return cached matrix inverse
	list(set = set, get = get, setinv = setinv, getinv = getinv)	
}


## Write a short comment describing this function
## Calculate inverse of cached matrix
## Use: x_inv <- cacheSolve(x)
## parameter: cached matrix
## TEST: x_inv %*% x$get() should return identity matrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()				# get current cached inverse matrix
	if (!is.null(inv)) {			# if one exists return from cache
		message("getting cached data")
		return(inv)					# return inverse matrix for cached matrix
	}
	data <- x$get()					# get original matrix from cache
	inv <- solve(data)				# calculate inverse of cached matrix
	x$setinv(inv)					# set cache inverse matrix
	inv								# return cached matrix inverse
}

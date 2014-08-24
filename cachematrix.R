## The following functions cache the inverse of a matrix, which is usually a costly computation.
## makeCacheMatrix() creates a special "matrix" object that can cache its inverse
## cacheSolve() computes the inverse of the special "matrix" returned by makeCacheMatrix()
## if the inverse has already been calculated, cachesolve() retrieve the inverse from the cache
## usage:
## mcache = makeCacheMatrix(matrix m)
## cacheSolve(mcache)



## This function creates a special "matrix" object that can cache its inverse
## actually it is a list containing a function to
## set()   : set the value of the matrix
## get()   : get the value of the matrx
## setinv(): set the value of the inverse
## getinv(): get the value of the inverse
makeCacheMatrix <- function(m = matrix()) {
	i <- NULL
	set <- function(y) {
		m <<- y
		i <<- NULL
	}
	get <- function() m
	setinv <- function(inverse) i <<- inverse
	getinv <- function() i
	list(set = set, get = get,
		setinv = setinv,
		getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with makeCacheMatrix
## it first checks to see if the inverse has already been calculated
## in this case it gets the inverse from the cache and skips the computation
## otherwise, it calculates the inverse of the data and sets this value in the cache via setinv()
cacheSolve <- function(x, ...) {
	i <- x$getinv()
	if(!is.null(i)) {
		message("getting cached inverse")
		return(i)
	}
	data <- x$get()
	## Return a matrix that is the inverse of 'x'
	i <- solve(data, ...)
	x$setinv(i)
	i
}


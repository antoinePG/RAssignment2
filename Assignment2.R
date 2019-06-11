## Create a special "matrix"
## i.e. an object that contains 4 getters/setters functions + 2 data objects
makeCacheMatrix <- function(mat = matrix()) {

	## Initialize inv (mat initialized as function argument) 
	inv <- NULL

	## set mat in parent fct with the param y & inv as NULL bc not cached
	set <- function(y) {
		mat <<- y
		inv <<- NULL
	}

	## get mat retrieved from parent environment i.e. makeCacheMatrix 
	get <- function() mat

	## set inv matrix with fct solve (inv in parent environment) 
	setinv <- function(solve) inv <<- solve

	## inv is the "getter" of the inverted matrix 
	getinv <- function() inv

	## create the special "matrix“
	## (assign getters/setters to elements of a list)
	list(set = set, get = get,
		setinv = setinv, 
		getinv = getinv)
}



## Function that computes the invert of a makeCacheMatrix object
## If invert exists in cache then invert is retrieved and not computed
cacheSolve <- function(mat,...) {

	## 1st attempt => retrieve inv from the makeCacheMatrix object argument 
	inv <- mat$getinv()

	## if different from NULL then a valid inv matrix has been cached and 
	## we return it with a message signaling it‘s been found in the cache
	if(!is.null(inv)) {
		message("Inverted matrix found in cache")
		return(inv)
	}

	## else we take the original matrix with get then compute its 
	## invert with solve and set it in the makeCacheMatrix object
	## and finally return the inverted matrix inv
	data_matrix <- mat$get()
	inv <- solve(data_matrix)
	mat$setinv(inv)
	inv
}


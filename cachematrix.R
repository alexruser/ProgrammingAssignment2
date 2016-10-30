## Below are two functions that are used to create a special object that stores an invertible 
## matrix and cache's its inverse.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 	Set the value of the matrix
## 	Get the value of the matrix
## 	Set the value of the inverse
## 	Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	s <- NULL
	set <- function(q) {
		x <<- q
		s <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) s <<- solve
	getsolve <- function() s
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets 
## the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of 
## the data and sets the value of the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data, ...)
	x$setsolve(s)

	s
	## Return a matrix that is the inverse of 'p'
}
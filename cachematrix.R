## Functions in this file both contribute to save some time of 
## potentially expensive oprations(in this case the inverse of a matrix)
## by caching the inverse in a special matrix container



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(inverse) inv <<- inverse
	getinv <- function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("getting cached data")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinv(inv)
	inv
}

## test drive

#create a 2x2 matrix
mat <- matrix(c(4, 3, 3, 2), 2, 2)  

#make it a special 'cacheMatrix'
m <- makeCacheMatrix(mat)           

#set the data inside the cached matrix with our matrix
m$set(mat) 

#retreive the inverse
inv <- cacheSolve(m)

#retreive the inverse again, should print out("getting cached data")
inv <- cacheSolve(m)

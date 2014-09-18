## The below functions are used in conjunction
## to cache the inverse of matrix.
## To test this, try
## matr <- makeCacheMatrix(matrix(c(1,2,2,1), nrow=2, ncol=2))
## matr$get() - displays the matrix in the argument
## matr$set(matrix(c(1,3,3,1), nrow=2, ncol=2)) can be used to set
## new matrix
## To compute the inverse, use cacheSolve(matr)
## First time, inverse is computed using solve function
## second call, to same gets it from cache 

## This function returns  
## list of set/get and setInverse/getInverse 
## functions , which can be called on assigned object


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(matr) {
		x <<- matr
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv<<- inverse
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## If inverse of matrix is cached,
## inverse is returned from cached variable inv
## otherwise inverse is set by using solve function
## on matrix x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached inverse matrix")
		return(inv)
	}
	matr <- x$get()
	inv <- solve(matr)
	x$setInverse(inv)
	inv
}

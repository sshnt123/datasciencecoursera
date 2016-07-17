## caching the inverse of a matrix which is considered as an extremely time and resource consuming process

## The first function makeCacheMatrix creates a special matrix that can cache its inverse
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
	x <<- y
	inv <<- NULL
	}
	get <- function()x
	setInv <- function(inverse)inv <<- inverse
	getInv <- function() inv
	list(set = set, get = get, setInv = setInv, getInv = getInv)
}



## This function computes the inverse of the special matrix created by the above function - "makeCacheMatrix". 
## if the inverse of the matrix has already been computed (and the matrix has not changed), then it should retrieve
## from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }	
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setInv(inv)
	inv

}

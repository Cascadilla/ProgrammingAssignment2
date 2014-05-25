## Matrix inversion is usually a time-consuming computation
## and it is beneficial to cache the inverse of a matrix
## rather than compute it repeatedly.
## Below are two functions that are used to create a special matrix object
## and cache its inverse.

## makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
		set <- function(y) {
				x <<- y
				i <<- NULL
		}
		get <- function() x
		setinv <- function(inv) i <<- inv
		getinv <- function() i
		list(set = set, get = get,
			 setinv = setinv,
			 getinv = getinv)
}

## cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.
## If the inverse has already been calculated and the matrix has not changed,
## then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}

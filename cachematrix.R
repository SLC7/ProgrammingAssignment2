

## This script contains two functions that are used to create a special
## 'matrix' object that stores a matrix and caches its inverse. Please note
## that the structure of the code was provided in the Assignment example by 
## Dr. Peng is utilized here. Minor changes were required for code modification 
## of the example to achieve the assignmen objectives. 

## The first function, `makeCacheMatrix` creates a special "matrix", 
## object that can cache its inverse. The output is a list of:
## (a) set the value of the matrix, (b) get the value of the matrix, 
## (c) set the value of the inverse of the matrix, (d) get the value 
## of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
			## Return a matrix 'x'
			n <- NULL
			set <- function(y = matrix()) {
					x <<- y
					n <<- NULL
			}
			get <- function() x
			setinverse <- function(solve) n <<- solve
			getinverse <- function() n
			
			## create a list to store the value of the matrix 'x',
			## and to store the value of the inverse of 'x',
			## with parameters to both 'set' and 'get' the matrix and its inverse
			
			list(set = set, get = get,
					setinverse = setinverse,
					getinverse = getinverse)
}


## The following function calculates the inverse of the special 'matrix'
## created in "makeCacheMatrix". First, the function checks to see if the 
## inverse has already been calculated. If the inverse exists, the function 
## gets the value from the cache and skips the calculation of the inverse. 
## If the inverse does not exist in the cache, then the function calculates 
## the inverse using the 'solve function and sets the value of the matrix 
## inverse in the cache via the 'setinverse' function.

cacheSolve <- function(x, ...) {
        ## Assign 'n' to be value in 'getinverse'
        n <- x$getinverse()
        ## checks if 'n' contains cached matrix inverse
        if(!is.null(n)) {
        		message("getting cached matrix inverse")
        		return(n)
        }
        matrix_inverse <- x$get()
        ## solves for matrix inverse
        n <- solve(matrix_inverse, ...)
        x$setinverse(n)
        n
}
